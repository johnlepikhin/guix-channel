;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Evgenii Lepikhin <johnlepikhin@gmail.com>
;;;
;;; This file is not part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

;;; Host monitoring: Prometheus, Alertmanager, Grafana and exporters in
;;; containers, plus textfile collectors for the node exporter of Guix.
;;;
;;; The containers are extensions of `oci-service-type', started by root
;;; (rootless podman would map the container users to other host users) on
;;; the host network.  Each listens on loopback unless told otherwise, runs
;;; as the unprivileged user its image is built for, and keeps its data in a
;;; host directory that the activation creates with that owner.
;;; Secrets are files placed by hand, owned by that user with mode 0400; the
;;; activation only warns about missing ones.
;;;
;;; Configuration files are written as JSON, which is valid YAML, and are
;;; mounted read-only at fixed paths in the containers.  A running container
;;; keeps its configuration until it is restarted: after a deploy, `herd
;;; restart' the changed ones.
;;;
;;; Images are pinned by digest with the tag kept for readability.  To
;;; upgrade one, look the digest of the new tag up, e.g. with `skopeo
;;; inspect docker://quay.io/prometheus/prometheus:vX.Y.Z', replace both
;;; and check the configuration with the new image's own tools.
;;;
;;; Alerts go to a webhook, normally a Home Assistant automation that turns
;;; them into push notifications.  The rule libraries at the end take every
;;; host-specific value as an argument.

(define-module (johnlepikhin system services monitoring)
  #:use-module (gnu packages admin)                ; smartmontools
  #:use-module (gnu packages base)                 ; coreutils
  #:use-module (gnu packages containers)           ; skopeo
  #:use-module (gnu packages guile)                ; guile-json-4
  #:use-module (gnu services)
  #:use-module (gnu services containers)
  #:use-module (gnu services mcron)
  #:use-module (guix diagnostics)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (johnlepikhin system services utils)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:export (alert-rule
            alert-rule?
            alert-rule-group
            alert-rule-group?
            inhibit-rule
            inhibit-rule?

            prometheus-scrape-job
            prometheus-scrape-job?
            blackbox-scrape-job
            prometheus-extension
            prometheus-extension?
            prometheus-configuration
            prometheus-configuration?
            prometheus-service-type

            alertmanager-route
            alertmanager-route?
            alertmanager-configuration
            alertmanager-configuration?
            alertmanager-service-type

            grafana-datasource
            grafana-datasource?
            grafana-dashboard
            grafana-dashboard?
            %grafana-node-exporter-dashboard
            %grafana-mktxp-dashboard
            grafana-configuration
            grafana-configuration?
            grafana-service-type

            blackbox-exporter-configuration
            blackbox-exporter-configuration?
            blackbox-http-module
            blackbox-http-module?
            blackbox-exporter-service-type

            mktxp-router
            mktxp-router?
            mktxp-configuration
            mktxp-configuration?
            mktxp-service-type

            loki-configuration
            loki-configuration?
            loki-service-type

            log-source
            log-source?
            alloy-configuration
            alloy-configuration?
            alloy-service-type

            %monitoring-images
            %node-textfile-directory
            node-textfile-collectors-configuration
            node-textfile-collectors-configuration?
            node-textfile-collectors-service-type

            host-alert-rules
            service-alert-rules
            stack-alert-rules
            network-alert-rules
            log-alert-rules
            backup-repository
            backup-repository?
            backup-repository-name
            backup-alert-rules
            home-assistant-alert-rules
            update-alert-rules
            prebuild-alert-rules))


;;;
;;; Helpers.
;;;

(define-syntax-rule (configuration-error message args ...)
  (raise (formatted-message message args ...)))

(define (duration? value)
  (and (string? value)
       (string-match "^([0-9]+(ms|s|m|h|d|w|y))+$" value)))

(define (sanitize-duration field)
  (lambda (value)
    (unless (duration? value)
      (configuration-error (G_ "~a: invalid duration: ~s") field value))
    value))

(define (sanitize-maybe-duration field)
  (lambda (value)
    (if value ((sanitize-duration field) value) value)))

(define %severities '(critical warning info))

(define (sanitize-severity value)
  (unless (memq value %severities)
    (configuration-error (G_ "alert severity must be one of ~s: ~s")
                         %severities value))
  value)

(define (number->config-string value)
  "Render VALUE without a fraction when it is integral, as PromQL and the
command lines expect."
  (if (and (real? value) (integer? value))
      (number->string (inexact->exact value))
      (number->string value)))

(define (json-file name value)
  "A file NAME holding VALUE as JSON; objects are alists with string keys,
arrays are vectors."
  (plain-file name (scm->json-string value #:pretty #t)))

(define* (file-tree name files #:key (directories '()))
  "A directory NAME holding copies of FILES, a list of relative path and
file-like pairs, and the empty DIRECTORIES.  Copies rather than store
symlinks, so that it can be mounted into a container on its own."
  (computed-file
   name
   (with-imported-modules '((guix build utils))
     #~(begin
         (use-modules (guix build utils))
         (for-each (lambda (directory)
                     (mkdir-p (string-append #$output "/" directory)))
                   '#$directories)
         (for-each (lambda (entry)
                     (let ((target (string-append #$output "/" (car entry))))
                       (mkdir-p (dirname target))
                       (copy-file (cdr entry) target)))
                   (list #$@(map (match-lambda
                                   ((path . file) #~(cons #$path #$file)))
                                 files)))))))

(define (container-user uid gid)
  (string-append (number->string uid) ":" (number->string gid)))


;;;
;;; Alert rules, shared by Prometheus and the Loki ruler.
;;;

(define-record-type* <alert-rule>
  alert-rule
  make-alert-rule
  alert-rule?
  (alert        alert-rule-alert)                  ;string
  (expr         alert-rule-expr)                   ;PromQL or LogQL string
  (for          alert-rule-for
                (default "0s")
                (sanitize (sanitize-duration "for")))
  (severity     alert-rule-severity                ;symbol in %severities
                (default 'warning)
                (sanitize sanitize-severity))
  ;; Label that inhibit rules select groups of alerts by, e.g. "hardware".
  (category     alert-rule-category
                (default #f))
  ;; One line, shown as the notification text; may use templates such as
  ;; {{ $labels.device }} and {{ $value }}.
  (summary      alert-rule-summary)
  (description  alert-rule-description
                (default #f))
  (labels       alert-rule-labels                  ;alist of strings
                (default '()))
  (annotations  alert-rule-annotations             ;alist of strings
                (default '())))

(define-record-type* <alert-rule-group>
  alert-rule-group
  make-alert-rule-group
  alert-rule-group?
  (name      alert-rule-group-name)                ;string
  (interval  alert-rule-group-interval
             (default #f)
             (sanitize (sanitize-maybe-duration "interval")))
  (rules     alert-rule-group-rules))              ;list of <alert-rule>

(define (alert-rule->json rule)
  (match-record rule <alert-rule>
    (alert expr for severity category summary description labels annotations)
    `(("alert" . ,alert)
      ("expr" . ,expr)
      ("for" . ,for)
      ("labels"
       ("severity" . ,(symbol->string severity))
       ,@(if category `(("category" . ,category)) '())
       ,@labels)
      ("annotations"
       ("summary" . ,summary)
       ,@(if description `(("description" . ,description)) '())
       ,@annotations))))

(define (alert-rule-groups-file name groups)
  (json-file
   name
   `(("groups"
      . ,(list->vector
          (map (lambda (group)
                 `(("name" . ,(alert-rule-group-name group))
                   ,@(match (alert-rule-group-interval group)
                       (#f '())
                       (interval `(("interval" . ,interval))))
                   ("rules"
                    . ,(list->vector
                        (map alert-rule->json
                             (alert-rule-group-rules group))))))
               groups))))))

;; Alerts matching TARGET-MATCHERS are muted while one matching
;; SOURCE-MATCHERS fires with the same values of the EQUAL labels.
;; Matchers are strings in the Alertmanager syntax: "alertname=\"X\"".
(define-record-type* <inhibit-rule>
  inhibit-rule
  make-inhibit-rule
  inhibit-rule?
  (source-matchers  inhibit-rule-source-matchers)  ;list of strings
  (target-matchers  inhibit-rule-target-matchers)  ;list of strings
  (equal            inhibit-rule-equal             ;list of label names
                    (default '())))

(define (inhibit-rule->json rule)
  (match-record rule <inhibit-rule> (source-matchers target-matchers equal)
    `(("source_matchers" . ,(list->vector source-matchers))
      ("target_matchers" . ,(list->vector target-matchers))
      ,@(if (null? equal) '() `(("equal" . ,(list->vector equal)))))))


;;;
;;; Prometheus.
;;;

;; Targets are "host:port" strings, or pairs of such a string and an alist
;; of labels of its own.
(define-record-type* <prometheus-scrape-job>
  prometheus-scrape-job
  make-prometheus-scrape-job
  prometheus-scrape-job?
  (name             prometheus-scrape-job-name)    ;string
  (targets          prometheus-scrape-job-targets)
  (metrics-path     prometheus-scrape-job-metrics-path
                    (default #f))
  (params           prometheus-scrape-job-params   ;alist of strings
                    (default '()))
  (interval         prometheus-scrape-job-interval
                    (default #f)
                    (sanitize (sanitize-maybe-duration "interval")))
  (timeout          prometheus-scrape-job-timeout
                    (default #f)
                    (sanitize (sanitize-maybe-duration "timeout")))
  (labels           prometheus-scrape-job-labels   ;alist of strings
                    (default '()))
  ;; A target exposing more samples than this fails its scrape instead of
  ;; growing the database without bound.
  (sample-limit     prometheus-scrape-job-sample-limit
                    (default 20000))
  (relabel-configs  prometheus-scrape-job-relabel-configs ;alists
                    (default '()))
  ;; Secret host file with a bearer token for the target, e.g. a Home
  ;; Assistant long-lived access token; owned by the Prometheus user, 0400.
  (credentials-file prometheus-scrape-job-credentials-file
                    (default #f))
  ;; "https" for targets that serve TLS.
  (scheme           prometheus-scrape-job-scheme
                    (default #f))
  ;; File-like CA certificate the targets' certificates are checked with.
  (ca-file          prometheus-scrape-job-ca-file
                    (default #f))
  ;; #f, or a user name and a secret host file with its password, for
  ;; basic authentication; the file is owned by the Prometheus user, 0400.
  (basic-auth       prometheus-scrape-job-basic-auth
                    (default #f)))

(define (scrape-job-credentials-path job)
  (string-append "/run/secrets/scrape-" (prometheus-scrape-job-name job)))

(define (scrape-job-ca-path job)
  (string-append "/run/secrets/scrape-" (prometheus-scrape-job-name job)
                 "-ca.crt"))

(define (scrape-job-password-path job)
  (string-append "/run/secrets/scrape-" (prometheus-scrape-job-name job)
                 "-password"))

(define (scrape-job-secret-files job)
  "The secret host files of JOB."
  (filter identity
          (list (prometheus-scrape-job-credentials-file job)
                (match (prometheus-scrape-job-basic-auth job)
                  ((_ . file) file)
                  (#f #f)))))

(define (scrape-job-volumes job)
  "The host files of JOB and where its container sees them."
  (filter-map
   (match-lambda
     ((#f . _) #f)
     ((file . path) (cons file (string-append path ":ro"))))
   `((,(prometheus-scrape-job-credentials-file job)
      . ,(scrape-job-credentials-path job))
     (,(prometheus-scrape-job-ca-file job) . ,(scrape-job-ca-path job))
     (,(match (prometheus-scrape-job-basic-auth job)
         ((_ . file) file)
         (#f #f))
      . ,(scrape-job-password-path job)))))

(define (scrape-job->json job)
  (match-record job <prometheus-scrape-job>
    (name targets metrics-path params interval timeout labels sample-limit
          relabel-configs credentials-file scheme ca-file basic-auth)
    (let ((plain (filter string? targets))
          (labelled (filter pair? targets)))
      `(("job_name" . ,name)
        ,@(if metrics-path `(("metrics_path" . ,metrics-path)) '())
        ,@(if (null? params)
              '()
              `(("params"
                 . ,(map (match-lambda
                           ((key . value) (cons key (vector value))))
                         params))))
        ,@(if interval `(("scrape_interval" . ,interval)) '())
        ,@(if timeout `(("scrape_timeout" . ,timeout)) '())
        ("sample_limit" . ,sample-limit)
        ("static_configs"
         . ,(list->vector
             (append
              (if (null? plain)
                  '()
                  (list `(("targets" . ,(list->vector plain))
                          ,@(if (null? labels)
                                '()
                                `(("labels" . ,labels))))))
              (map (match-lambda
                     ((target . target-labels)
                      `(("targets" . ,(vector target))
                        ("labels" . ,(append labels target-labels)))))
                   labelled))))
        ,@(if (null? relabel-configs)
              '()
              `(("relabel_configs" . ,(list->vector relabel-configs))))
        ,@(if credentials-file
              `(("authorization"
                 ("type" . "Bearer")
                 ("credentials_file" . ,(scrape-job-credentials-path job))))
              '())
        ,@(if scheme `(("scheme" . ,scheme)) '())
        ,@(if ca-file
              `(("tls_config" ("ca_file" . ,(scrape-job-ca-path job))))
              '())
        ,@(match basic-auth
            (#f '())
            ((user . _)
             `(("basic_auth"
                ("username" . ,user)
                ("password_file" . ,(scrape-job-password-path job))))))))))

(define* (blackbox-scrape-job name module targets
                              #:key (exporter "127.0.0.1:9115")
                              (interval #f) (labels '()))
  "Return a scrape job probing TARGETS with the blackbox exporter MODULE.
Each target becomes the instance label of its probe."
  (prometheus-scrape-job
   (name name)
   (targets targets)
   (metrics-path "/probe")
   (params `(("module" . ,module)))
   (interval interval)
   (labels labels)
   (relabel-configs
    `((("source_labels" . #("__address__"))
       ("target_label" . "__param_target"))
      (("source_labels" . #("__param_target"))
       ("target_label" . "instance"))
      (("target_label" . "__address__")
       ("replacement" . ,exporter))))))

;; What other services add to Prometheus.
(define-record-type* <prometheus-extension>
  prometheus-extension
  make-prometheus-extension
  prometheus-extension?
  (scrape-jobs    prometheus-extension-scrape-jobs
                  (default '()))
  (rule-groups    prometheus-extension-rule-groups
                  (default '()))
  ;; "host:port" of Alertmanagers to send alerts to.
  (alertmanagers  prometheus-extension-alertmanagers
                  (default '())))

(define %prometheus-image
  "quay.io/prometheus/prometheus:v3.15.1@sha256:d445d1806b38f85b5c394a1b87f94113fde0473b45245922c2e8b1bd41fa68ae")

(define-record-type* <prometheus-configuration>
  prometheus-configuration
  make-prometheus-configuration
  prometheus-configuration?
  (image                prometheus-configuration-image
                        (default %prometheus-image))
  (uid                  prometheus-configuration-uid    ;"nobody" of the image
                        (default 65534))
  (gid                  prometheus-configuration-gid
                        (default 65534))
  (listen-address       prometheus-configuration-listen-address
                        (default "127.0.0.1:9090"))
  (data-directory       prometheus-configuration-data-directory
                        (default "/var/lib/prometheus"))
  (retention-time       prometheus-configuration-retention-time
                        (default "90d")
                        (sanitize (sanitize-duration "retention-time")))
  ;; Hard limit on the database size, as for --storage.tsdb.retention.size.
  (retention-size       prometheus-configuration-retention-size
                        (default "20GB"))
  (scrape-interval      prometheus-configuration-scrape-interval
                        (default "30s")
                        (sanitize (sanitize-duration "scrape-interval")))
  (evaluation-interval  prometheus-configuration-evaluation-interval
                        (default "30s")
                        (sanitize (sanitize-duration "evaluation-interval")))
  (scrape-jobs          prometheus-configuration-scrape-jobs
                        (default '()))
  (rule-groups          prometheus-configuration-rule-groups
                        (default '()))
  (alertmanagers        prometheus-configuration-alertmanagers
                        (default '()))
  (memory               prometheus-configuration-memory
                        (default "2g")))

(define %prometheus-config-path "/etc/prometheus/prometheus.yml")
(define %prometheus-rules-path "/etc/prometheus/rules.yml")

(define (prometheus-self-scrape-job config)
  (prometheus-scrape-job
   (name "prometheus")
   (targets (list (prometheus-configuration-listen-address config)))))

(define (prometheus-config-file config)
  (match-record config <prometheus-configuration>
    (scrape-interval evaluation-interval scrape-jobs alertmanagers)
    (json-file
     "prometheus.yml"
     `(("global"
        ("scrape_interval" . ,scrape-interval)
        ("evaluation_interval" . ,evaluation-interval))
       ("rule_files" . #(,%prometheus-rules-path))
       ,@(if (null? alertmanagers)
             '()
             `(("alerting"
                ("alertmanagers"
                 . #((("static_configs"
                       . #((("targets"
                             . ,(list->vector alertmanagers)))))))))))
       ("scrape_configs"
        . ,(list->vector
            (map scrape-job->json
                 (cons (prometheus-self-scrape-job config)
                       scrape-jobs))))))))

(define (prometheus-rules-file config)
  (alert-rule-groups-file "prometheus-rules.yml"
                          (prometheus-configuration-rule-groups config)))

(define (prometheus-oci-extension config)
  (match-record config <prometheus-configuration>
    (image uid gid listen-address data-directory retention-time
           retention-size memory)
    (oci-extension
     (containers
      (list (oci-container-configuration
             (provision "prometheus")
             (user "root")
             (group "root")
             (image image)
             (network "host")
             (container-user (container-user uid gid))
             (respawn? #t)
             (log-file "/var/log/prometheus.log")
             (volumes
              `((,(prometheus-config-file config)
                 . ,(string-append %prometheus-config-path ":ro"))
                (,(prometheus-rules-file config)
                 . ,(string-append %prometheus-rules-path ":ro"))
                (,data-directory . "/prometheus")
                ,@(append-map scrape-job-volumes
                              (prometheus-configuration-scrape-jobs
                               config))))
             (command
              (list (string-append "--config.file=" %prometheus-config-path)
                    "--storage.tsdb.path=/prometheus"
                    (string-append "--storage.tsdb.retention.time="
                                   retention-time)
                    (string-append "--storage.tsdb.retention.size="
                                   retention-size)
                    (string-append "--web.listen-address=" listen-address)))
             (extra-arguments (list "--memory" memory))))))))

(define (prometheus-activation config)
  (match-record config <prometheus-configuration>
    (data-directory uid gid scrape-jobs)
    #~(begin
        #$(data-directory-activation data-directory uid gid)
        #$@(map (lambda (file) (secret-file-check file uid))
                (append-map scrape-job-secret-files scrape-jobs)))))

(define (prometheus-extend config extensions)
  (prometheus-configuration
   (inherit config)
   (scrape-jobs (append (prometheus-configuration-scrape-jobs config)
                        (append-map prometheus-extension-scrape-jobs
                                    extensions)))
   (rule-groups (append (prometheus-configuration-rule-groups config)
                        (append-map prometheus-extension-rule-groups
                                    extensions)))
   (alertmanagers (delete-duplicates
                   (append (prometheus-configuration-alertmanagers config)
                           (append-map prometheus-extension-alertmanagers
                                       extensions))))))

(define prometheus-service-type
  (service-type
   (name 'prometheus)
   (extensions
    (list (service-extension oci-service-type prometheus-oci-extension)
          (service-extension activation-service-type
                             prometheus-activation)))
   (compose identity)
   (extend prometheus-extend)
   (default-value (prometheus-configuration))
   (description "Run Prometheus in a container.  It scrapes itself; other
services add their scrape jobs, rules and Alertmanagers by extending it.")))


;;;
;;; Alertmanager.
;;;

(define %alertmanager-image
  "quay.io/prometheus/alertmanager:v0.34.1@sha256:84967b9b7ba45e38a9278d3e594305f43d4993c310df3905b51138b816c365f3")

(define-record-type* <alertmanager-configuration>
  alertmanager-configuration
  make-alertmanager-configuration
  alertmanager-configuration?
  (image                     alertmanager-configuration-image
                             (default %alertmanager-image))
  (uid                       alertmanager-configuration-uid
                             (default 65534))
  (gid                       alertmanager-configuration-gid
                             (default 65534))
  (listen-address            alertmanager-configuration-listen-address
                             (default "127.0.0.1:9093"))
  (data-directory            alertmanager-configuration-data-directory
                             (default "/var/lib/alertmanager"))
  ;; Secret file with the URL the alerts are posted to, e.g. a Home
  ;; Assistant webhook, whose ID is all the authentication it has.
  (webhook-url-file          alertmanager-configuration-webhook-url-file
                             (default "/etc/alertmanager/webhook-url"))
  ;; The default sends every alert on its own, so that a notification
  ;; stands for one alert and resolves with it.
  (group-by                  alertmanager-configuration-group-by
                             (default '("...")))
  (group-wait                alertmanager-configuration-group-wait
                             (default "30s")
                             (sanitize (sanitize-duration "group-wait")))
  (group-interval            alertmanager-configuration-group-interval
                             (default "5m")
                             (sanitize (sanitize-duration "group-interval")))
  (repeat-interval-critical  alertmanager-configuration-repeat-interval-critical
                             (default "4h")
                             (sanitize
                              (sanitize-duration "repeat-interval-critical")))
  (repeat-interval-warning   alertmanager-configuration-repeat-interval-warning
                             (default "24h")
                             (sanitize
                              (sanitize-duration "repeat-interval-warning")))
  ;; Info alerts, such as the Watchdog of `stack-alert-rules', repeat this
  ;; often, so that the receiver can tell a dead pipeline from a quiet one.
  (repeat-interval-info      alertmanager-configuration-repeat-interval-info
                             (default "1h")
                             (sanitize
                              (sanitize-duration "repeat-interval-info")))
  (inhibit-rules             alertmanager-configuration-inhibit-rules
                             (default '()))
  ;; <alertmanager-route> records, tried before the routes by severity.
  (extra-routes              alertmanager-configuration-extra-routes
                             (default '()))
  (memory                    alertmanager-configuration-memory
                             (default "256m")))

(define-record-type* <alertmanager-route>
  alertmanager-route
  make-alertmanager-route
  alertmanager-route?
  (matchers         alertmanager-route-matchers)    ;list of strings
  (repeat-interval  alertmanager-route-repeat-interval
                    (sanitize (sanitize-duration "repeat-interval")))
  ;; #f to inherit the grouping of the root route, or a list of label
  ;; names: '() puts all the matching alerts in one notification.
  (group-by         alertmanager-route-group-by
                    (default #f)))

(define (alertmanager-route->json route)
  (match-record route <alertmanager-route>
    (matchers repeat-interval group-by)
    `(("matchers" . ,(list->vector matchers))
      ("repeat_interval" . ,repeat-interval)
      ,@(if group-by
            `(("group_by" . ,(list->vector group-by)))
            '()))))

(define %alertmanager-config-path "/etc/alertmanager/alertmanager.yml")
(define %alertmanager-webhook-url-path "/run/secrets/webhook-url")

(define (alertmanager-config-file config)
  (match-record config <alertmanager-configuration>
    (group-by group-wait group-interval repeat-interval-critical
              repeat-interval-warning repeat-interval-info inhibit-rules
              extra-routes)
    (json-file
     "alertmanager.yml"
     `(("route"
        ("receiver" . "webhook")
        ("group_by" . ,(list->vector group-by))
        ("group_wait" . ,group-wait)
        ("group_interval" . ,group-interval)
        ("repeat_interval" . ,repeat-interval-warning)
        ("routes"
         . ,(list->vector
             (append
              (map alertmanager-route->json extra-routes)
              `((("matchers" . #("severity=\"info\""))
                 ("group_wait" . "0s")
                 ("repeat_interval" . ,repeat-interval-info))
                (("matchers" . #("severity=\"critical\""))
                 ("group_wait" . "10s")
                 ("repeat_interval" . ,repeat-interval-critical)))))))
       ("receivers"
        . #((("name" . "webhook")
             ("webhook_configs"
              . #((("url_file" . ,%alertmanager-webhook-url-path)
                   ("send_resolved" . #t)))))))
       ,@(if (null? inhibit-rules)
             '()
             `(("inhibit_rules"
                . ,(list->vector (map inhibit-rule->json inhibit-rules)))))))))

(define (alertmanager-oci-extension config)
  (match-record config <alertmanager-configuration>
    (image uid gid listen-address data-directory webhook-url-file memory)
    (oci-extension
     (containers
      (list (oci-container-configuration
             (provision "alertmanager")
             (user "root")
             (group "root")
             (image image)
             (network "host")
             (container-user (container-user uid gid))
             (respawn? #t)
             (log-file "/var/log/alertmanager.log")
             (volumes
              `((,(alertmanager-config-file config)
                 . ,(string-append %alertmanager-config-path ":ro"))
                (,webhook-url-file
                 . ,(string-append %alertmanager-webhook-url-path ":ro"))
                (,data-directory . "/alertmanager")))
             (command
              (list (string-append "--config.file="
                                   %alertmanager-config-path)
                    "--storage.path=/alertmanager"
                    (string-append "--web.listen-address=" listen-address)
                    ;; A single instance: no gossip port on all interfaces.
                    "--cluster.listen-address="))
             (extra-arguments (list "--memory" memory))))))))

(define (alertmanager-activation config)
  (match-record config <alertmanager-configuration>
    (data-directory uid gid webhook-url-file)
    #~(begin
        #$(data-directory-activation data-directory uid gid)
        #$(secret-file-check webhook-url-file uid))))

(define (alertmanager-prometheus-extension config)
  (let ((address (alertmanager-configuration-listen-address config)))
    (prometheus-extension
     (scrape-jobs (list (prometheus-scrape-job
                         (name "alertmanager")
                         (targets (list address)))))
     (alertmanagers (list address)))))

(define alertmanager-service-type
  (service-type
   (name 'alertmanager)
   (extensions
    (list (service-extension oci-service-type alertmanager-oci-extension)
          (service-extension activation-service-type
                             alertmanager-activation)
          (service-extension prometheus-service-type
                             alertmanager-prometheus-extension)))
   (default-value (alertmanager-configuration))
   (description "Run Alertmanager in a container, posting every alert and
its resolution to a webhook whose URL is kept in a secret file.  Critical
alerts repeat more often than warnings.")))


;;;
;;; Grafana.
;;;

(define %grafana-image
  "docker.io/grafana/grafana:13.2.4@sha256:7b22f3edf947329d75eba7a62d9f6ea1ac1e786736813ef7a98cfb275a52e49b")

(define-record-type* <grafana-datasource>
  grafana-datasource
  make-grafana-datasource
  grafana-datasource?
  (name       grafana-datasource-name)             ;string
  ;; Stable identifier that dashboards refer to.
  (uid        grafana-datasource-uid)              ;string
  (type       grafana-datasource-type)             ;"prometheus", "loki"…
  (url        grafana-datasource-url)              ;string
  (default?   grafana-datasource-default?
              (default #f))
  (json-data  grafana-datasource-json-data         ;alist
              (default '())))

(define-record-type* <grafana-dashboard>
  grafana-dashboard
  make-grafana-dashboard
  grafana-dashboard?
  (name         grafana-dashboard-name)            ;file name, "x.json"
  (file         grafana-dashboard-file)            ;file-like
  ;; Dashboards exported for sharing refer to their datasources as inputs,
  ;; ${DS_NAME}: an alist of such names to datasource uids.
  (datasources  grafana-dashboard-datasources
                (default '())))

(define (grafana-dashboard-json dashboard)
  "The JSON file of DASHBOARD, its datasource inputs replaced by uids."
  (match-record dashboard <grafana-dashboard> (name file datasources)
    (if (null? datasources)
        file
        (computed-file
         name
         (with-imported-modules '((guix build utils))
           #~(begin
               (use-modules (guix build utils))
               (copy-file #$file #$output)
               (chmod #$output #o644)
               (for-each (lambda (input)
                           (substitute* #$output
                             (((string-append "\\$\\{" (car input) "\\}"))
                              (cdr input))))
                         '#$datasources)))))))

;; "Node Exporter Full", whose datasource is a variable defaulting to the
;; default Prometheus datasource.
(define %grafana-node-exporter-dashboard
  (grafana-dashboard
   (name "node-exporter-full.json")
   (file (origin
           (method url-fetch)
           (uri "https://grafana.com/api/dashboards/1860/revisions/45/download")
           (file-name "grafana-dashboard-node-exporter-full-45.json")
           (sha256
            (base32
             "11hrll7fm626ikbva5md4gm0rca537vp4xsxa9sxl1pk15s6nk0q"))))))

;; "Mikrotik MKTXP Exporter", for `mktxp-service-type'.
(define %grafana-mktxp-dashboard
  (grafana-dashboard
   (name "mktxp.json")
   (file (origin
           (method url-fetch)
           (uri "https://grafana.com/api/dashboards/13679/revisions/32/download")
           (file-name "grafana-dashboard-mktxp-32.json")
           (sha256
            (base32
             "09dxx6s11qhnpv6ckn8n84m6lqxrziwvsi8gxqzlcm7vs3zgjwiz"))))
   (datasources '(("DS_PROMETHEUS" . "prometheus")))))

(define-record-type* <grafana-configuration>
  grafana-configuration
  make-grafana-configuration
  grafana-configuration?
  (image                grafana-configuration-image
                        (default %grafana-image))
  (uid                  grafana-configuration-uid
                        (default 472))
  (gid                  grafana-configuration-gid
                        (default 0))
  ;; Grafana is the one service meant for people: give it the LAN address.
  (http-address         grafana-configuration-http-address
                        (default "127.0.0.1"))
  (http-port            grafana-configuration-http-port
                        (default 3001))
  (data-directory       grafana-configuration-data-directory
                        (default "/var/lib/grafana"))
  ;; Secret file with the password of the "admin" user, without a trailing
  ;; newline.  Grafana sets it when it creates its database, on the first
  ;; start; later changes go through its web interface.
  (admin-password-file  grafana-configuration-admin-password-file
                        (default "/etc/grafana/admin-password"))
  (datasources          grafana-configuration-datasources
                        (default '()))
  (dashboards           grafana-configuration-dashboards
                        (default (list %grafana-node-exporter-dashboard)))
  ;; Grafana 13 settles at about 450 MB with a few dashboards.
  (memory               grafana-configuration-memory
                        (default "1g")))

(define %grafana-admin-password-path "/run/secrets/admin-password")
(define %grafana-provisioning-path "/etc/grafana/provisioning")

(define (grafana-datasource->json datasource)
  (match-record datasource <grafana-datasource>
    (name uid type url default? json-data)
    `(("name" . ,name)
      ("uid" . ,uid)
      ("type" . ,type)
      ("url" . ,url)
      ("access" . "proxy")
      ("isDefault" . ,default?)
      ("editable" . #f)
      ,@(if (null? json-data) '() `(("jsonData" . ,json-data))))))

(define (grafana-provisioning config)
  (match-record config <grafana-configuration> (datasources dashboards)
    (file-tree
     "grafana-provisioning"
     `(("datasources/datasources.yaml"
        . ,(json-file "datasources.yaml"
                      `(("apiVersion" . 1)
                        ("datasources"
                         . ,(list->vector
                             (map grafana-datasource->json datasources))))))
       ("dashboards/dashboards.yaml"
        . ,(json-file
            "dashboards.yaml"
            `(("apiVersion" . 1)
              ("providers"
               . #((("name" . "provisioned")
                    ("type" . "file")
                    ("disableDeletion" . #t)
                    ("allowUiUpdates" . #f)
                    ("options"
                     ("path"
                      . ,(string-append %grafana-provisioning-path
                                        "/dashboards/json")))))))))
       ,@(map (lambda (dashboard)
                (cons (string-append "dashboards/json/"
                                     (grafana-dashboard-name dashboard))
                      (grafana-dashboard-json dashboard)))
              dashboards))
     ;; Grafana logs errors about the provisioning directories it misses.
     #:directories '("plugins" "alerting"))))

(define (grafana-oci-extension config)
  (match-record config <grafana-configuration>
    (image uid gid http-address http-port data-directory admin-password-file
           memory)
    (oci-extension
     (containers
      (list (oci-container-configuration
             (provision "grafana")
             (user "root")
             (group "root")
             (image image)
             (network "host")
             (container-user (container-user uid gid))
             (respawn? #t)
             (log-file "/var/log/grafana.log")
             (environment
              `(("GF_SERVER_HTTP_ADDR" . ,http-address)
                ("GF_SERVER_HTTP_PORT" . ,(number->string http-port))
                ("GF_SECURITY_ADMIN_PASSWORD__FILE"
                 . ,%grafana-admin-password-path)
                ("GF_PATHS_PROVISIONING" . ,%grafana-provisioning-path)
                ("GF_USERS_ALLOW_SIGN_UP" . "false")
                ;; Alerts are Prometheus' job.
                ("GF_UNIFIED_ALERTING_ENABLED" . "false")
                ("GF_ANALYTICS_REPORTING_ENABLED" . "false")
                ("GF_ANALYTICS_CHECK_FOR_UPDATES" . "false")
                ("GF_ANALYTICS_CHECK_FOR_PLUGIN_UPDATES" . "false")
                ("GF_NEWS_NEWS_FEED_ENABLED" . "false")))
             (volumes
              `((,(grafana-provisioning config)
                 . ,(string-append %grafana-provisioning-path ":ro"))
                (,admin-password-file
                 . ,(string-append %grafana-admin-password-path ":ro"))
                (,data-directory . "/var/lib/grafana")))
             (extra-arguments (list "--memory" memory))))))))

(define (grafana-activation config)
  (match-record config <grafana-configuration>
    (data-directory uid gid admin-password-file)
    #~(begin
        #$(data-directory-activation data-directory uid gid)
        #$(secret-file-check admin-password-file uid))))

(define (grafana-prometheus-extension config)
  (match-record config <grafana-configuration> (http-address http-port)
    (prometheus-extension
     (scrape-jobs
      (list (prometheus-scrape-job
             (name "grafana")
             (targets (list (string-append http-address ":"
                                           (number->string http-port))))))))))

(define grafana-service-type
  (service-type
   (name 'grafana)
   (extensions
    (list (service-extension oci-service-type grafana-oci-extension)
          (service-extension activation-service-type grafana-activation)
          (service-extension prometheus-service-type
                             grafana-prometheus-extension)))
   (default-value (grafana-configuration))
   (description "Run Grafana in a container with provisioned, read-only
datasources and dashboards.  Its own alerting is off.")))


;;;
;;; Blackbox exporter.
;;;

(define %blackbox-exporter-image
  "quay.io/prometheus/blackbox-exporter:v0.29.1@sha256:66eae352a79e6f677aeddd8c25a9bdb017586dc5fbd230f22329213d891f94bd")

(define-record-type* <blackbox-exporter-configuration>
  blackbox-exporter-configuration
  make-blackbox-exporter-configuration
  blackbox-exporter-configuration?
  (image           blackbox-exporter-configuration-image
                   (default %blackbox-exporter-image))
  (listen-address  blackbox-exporter-configuration-listen-address
                   (default "127.0.0.1:9115"))
  (timeout         blackbox-exporter-configuration-timeout
                   (default "5s")
                   (sanitize (sanitize-duration "timeout")))
  (memory          blackbox-exporter-configuration-memory
                   (default "128m"))
  ;; More HTTP modules, as <blackbox-http-module> records.
  (extra-modules   blackbox-exporter-configuration-extra-modules
                   (default '())))

;; An HTTP probe module, e.g. for a service with a self-signed certificate,
;; whose expiry the probe then reports as probe_ssl_earliest_cert_expiry.
(define-record-type* <blackbox-http-module>
  blackbox-http-module
  make-blackbox-http-module
  blackbox-http-module?
  (name                blackbox-http-module-name)     ;string
  ;; File-like CA certificate the target's certificate is checked with.
  (ca-file             blackbox-http-module-ca-file
                       (default #f))
  ;; Status codes taken as success; empty means any 2xx.
  (valid-status-codes  blackbox-http-module-valid-status-codes
                       (default '())))

(define (blackbox-module-ca-path module)
  (string-append "/etc/blackbox_exporter/ca-"
                 (blackbox-http-module-name module) ".crt"))

(define %blackbox-exporter-config-path "/etc/blackbox_exporter/config.yml")

;; Modules for `blackbox-scrape-job': http_2xx, tcp_connect and icmp, all
;; over IPv4.
(define (blackbox-exporter-config-file config)
  (let ((timeout (blackbox-exporter-configuration-timeout config)))
    (json-file
     "blackbox.yml"
     `(("modules"
        ("http_2xx"
         ("prober" . "http")
         ("timeout" . ,timeout)
         ("http" ("preferred_ip_protocol" . "ip4")))
        ("tcp_connect"
         ("prober" . "tcp")
         ("timeout" . ,timeout)
         ("tcp" ("preferred_ip_protocol" . "ip4")))
        ("icmp"
         ("prober" . "icmp")
         ("timeout" . ,timeout)
         ("icmp" ("preferred_ip_protocol" . "ip4")))
        ,@(map (lambda (module)
                 (match-record module <blackbox-http-module>
                   (name ca-file valid-status-codes)
                   `(,name
                     ("prober" . "http")
                     ("timeout" . ,timeout)
                     ("http"
                      ("preferred_ip_protocol" . "ip4")
                      ,@(if (null? valid-status-codes)
                            '()
                            `(("valid_status_codes"
                               . ,(list->vector valid-status-codes))))
                      ,@(if ca-file
                            `(("tls_config"
                               ("ca_file"
                                . ,(blackbox-module-ca-path module))))
                            '())))))
               (blackbox-exporter-configuration-extra-modules config)))))))

(define (blackbox-exporter-oci-extension config)
  (match-record config <blackbox-exporter-configuration>
    (image listen-address memory)
    (oci-extension
     (containers
      (list (oci-container-configuration
             (provision "blackbox-exporter")
             (user "root")
             (group "root")
             (image image)
             (network "host")
             (respawn? #t)
             (log-file "/var/log/blackbox-exporter.log")
             (volumes
              `((,(blackbox-exporter-config-file config)
                 . ,(string-append %blackbox-exporter-config-path ":ro"))
                ,@(filter-map
                   (lambda (module)
                     (and (blackbox-http-module-ca-file module)
                          (cons (blackbox-http-module-ca-file module)
                                (string-append
                                 (blackbox-module-ca-path module) ":ro"))))
                   (blackbox-exporter-configuration-extra-modules config))))
             (command
              (list (string-append "--config.file="
                                   %blackbox-exporter-config-path)
                    (string-append "--web.listen-address=" listen-address)))
             ;; ICMP probes need raw sockets, nothing else does.
             (extra-arguments
              (list "--cap-drop" "ALL" "--cap-add" "NET_RAW"
                    "--memory" memory))))))))

(define (blackbox-exporter-prometheus-extension config)
  (prometheus-extension
   (scrape-jobs
    (list (prometheus-scrape-job
           (name "blackbox-exporter")
           (targets (list (blackbox-exporter-configuration-listen-address
                           config))))))))

(define blackbox-exporter-service-type
  (service-type
   (name 'blackbox-exporter)
   (extensions
    (list (service-extension oci-service-type
                             blackbox-exporter-oci-extension)
          (service-extension prometheus-service-type
                             blackbox-exporter-prometheus-extension)))
   (default-value (blackbox-exporter-configuration))
   (description "Run the Prometheus blackbox exporter in a container, for
HTTP, TCP and ICMP probes declared with @code{blackbox-scrape-job}.")))


;;;
;;; mktxp, the MikroTik RouterOS exporter.
;;;

(define %mktxp-image
  "ghcr.io/akpw/mktxp:2.1.0@sha256:4cbb31321914c8a0a95cfed8edbf2647f681311f1926bf06deb8866d40baaa1c")

;; mktxp wants every key of its configuration files and tries to write the
;; missing ones back, which fails on the read-only mount: these are the
;; defaults of the image, apart from the probe endpoint, which is off.
(define %mktxp-system-settings
  '(("socket_timeout" . 5)
    ("initial_delay_on_failure" . 120)
    ("max_delay_on_failure" . 900)
    ("delay_inc_div" . 5)
    ("bandwidth" . #f)
    ("bandwidth_test_dns_server" . "8.8.8.8")
    ("bandwidth_test_interval" . 600)
    ("minimal_collect_interval" . 5)
    ("verbose_mode" . #f)
    ("fetch_routers_in_parallel" . #f)
    ("max_worker_threads" . 5)
    ("max_scrape_duration" . 30)
    ("total_max_scrape_duration" . 90)
    ("http_server_threads" . 16)
    ("persistent_router_connection_pool" . #t)
    ("persistent_dhcp_cache" . #t)
    ("compact_default_conf_values" . #f)
    ("prometheus_headers_deduplication" . #f)
    ("probe_connection_pool" . #f)
    ("probe_connection_pool_ttl" . 300)
    ("probe_connection_pool_max_size" . 128)
    ("enable_probe" . #f)
    ("probe_allowed_targets" . "None")))

(define %mktxp-diag-settings
  '(("low_signal_threshold" . -75)
    ("min_signal_threshold" . -60)
    ("low_rate_threshold" . "'18M'")
    ("recent_duration" . "'15m'")
    ("top_connections_count" . 10)
    ("rate_above_threshold" . "'1M'")
    ("degraded_threshold" . "'100M'")))

;; The [default] router settings of the image, with only the cheap
;; collectors on: the
;; interfaces, their link state and rate, the routes and the system
;; resources, which mktxp always collects.  Small routers answer slowly to
;; the others.
(define %mktxp-router-settings
  '(("enabled" . #t)
    ("module_only" . #f)
    ("probe_allowed_targets" . "None")
    ("custom_labels" . "None")
    ("use_ssl" . #f)
    ("no_ssl_certificate" . #f)
    ("ssl_certificate_verify" . #f)
    ("ssl_check_hostname" . #t)
    ("ssl_ca_file" . "\"\"")
    ("plaintext_login" . #t)
    ("health" . #t)
    ("installed_packages" . #f)
    ("dhcp" . #f)
    ("dhcp_lease" . #f)
    ("connections" . #f)
    ("connection_stats" . #f)
    ("connection_stats_destinations" . #f)
    ("interface" . #t)
    ("interface_with_default_name" . #f)
    ("routerboard" . #f)
    ("wireguard_peers" . #f)
    ("bridge_vlan" . #f)
    ("route" . #t)
    ("pool" . #f)
    ("firewall" . #f)
    ("neighbor" . #f)
    ("address_list" . "None")
    ("address_list_entries" . #f)
    ("total_address_list_counts" . #f)
    ("dns" . #f)
    ("ipv6_route" . #f)
    ("ipv6_pool" . #f)
    ("ipv6_firewall" . #f)
    ("ipv6_neighbor" . #f)
    ("ipv6_address_list" . "None")
    ("ipv6_address_list_entries" . #f)
    ("ipv6_total_address_list_counts" . #f)
    ("poe" . #f)
    ("monitor" . #t)
    ("netwatch" . #f)
    ("public_ip" . #f)
    ("wireless" . #f)
    ("wireless_clients" . #f)
    ("capsman" . #f)
    ("capsman_clients" . #f)
    ("w60g" . #f)
    ("eoip" . #f)
    ("gre" . #f)
    ("ipip" . #f)
    ("lte" . #f)
    ("ipsec" . #f)
    ("switch_port" . #f)
    ("kid_control_assigned" . #f)
    ("kid_control_dynamic" . #f)
    ("user" . #f)
    ("queue" . #f)
    ("bfd" . #f)
    ("bgp" . #f)
    ("routing_stats" . #f)
    ("certificate" . #f)
    ("container" . #f)
    ("remote_dhcp_entry" . "None")
    ("remote_capsman_entry" . "None")
    ("interface_name_format" . "name")
    ("check_for_updates" . #f)
    ("rsc_ssh_port" . "None")
    ("rsc_ssh_user" . "None")))

(define-record-type* <mktxp-router>
  mktxp-router
  make-mktxp-router
  mktxp-router?
  (name      mktxp-router-name)                    ;routerboard_name label
  (address   mktxp-router-address)                 ;string
  (port      mktxp-router-port
             (default 8728))
  ;; Overrides of %mktxp-router-settings, e.g. '(("health" . #f)) for a
  ;; router without health sensors.
  (settings  mktxp-router-settings
             (default '())))

(define-record-type* <mktxp-configuration>
  mktxp-configuration
  make-mktxp-configuration
  mktxp-configuration?
  (image             mktxp-configuration-image
                     (default %mktxp-image))
  (uid               mktxp-configuration-uid
                     (default 65534))
  (gid               mktxp-configuration-gid
                     (default 65534))
  (listen-address    mktxp-configuration-listen-address
                     (default "127.0.0.1:49090"))
  ;; Secret YAML file with the "username" and "password" of a RouterOS user
  ;; of a group with the policies read and api only.
  (credentials-file  mktxp-configuration-credentials-file
                     (default "/etc/mktxp/credentials.yml"))
  (routers           mktxp-configuration-routers)  ;list of <mktxp-router>
  ;; Small routers take seconds to answer; scraping more often than this
  ;; only loads them.
  (scrape-interval   mktxp-configuration-scrape-interval
                     (default "60s")
                     (sanitize (sanitize-duration "scrape-interval")))
  (memory            mktxp-configuration-memory
                     (default "256m")))

(define %mktxp-config-path "/etc/mktxp")
(define %mktxp-credentials-path "/run/secrets/mktxp-credentials.yml")

(define (ini-value value)
  (cond ((eq? value #t) "True")
        ((eq? value #f) "False")
        ((number? value) (number->config-string value))
        (else value)))

(define (ini-section name settings)
  (string-append
   "[" name "]\n"
   (string-concatenate
    (map (match-lambda
           ((key . value)
            (string-append "    " key " = " (ini-value value) "\n")))
         settings))
   "\n"))

(define (merge-settings defaults overrides)
  "DEFAULTS with the values of OVERRIDES, in the order of DEFAULTS."
  (map (match-lambda
         ((key . value)
          (cons key (match (assoc key overrides)
                      ((_ . override) override)
                      (#f value)))))
       defaults))

(define (mktxp-config-directory config)
  (match-record config <mktxp-configuration> (listen-address routers)
    (file-tree
     "mktxp-config"
     `(("_mktxp.conf"
        . ,(plain-file
            "_mktxp.conf"
            (string-append
             (ini-section "MKTXP"
                          (cons `("listen" . ,(string-append
                                               "'" listen-address "'"))
                                %mktxp-system-settings))
             (ini-section "DIAG" %mktxp-diag-settings))))
       ("mktxp.conf"
        . ,(plain-file
            "mktxp.conf"
            (string-concatenate
             (cons
              (ini-section "default"
                           `(("hostname" . "localhost")
                             ("port" . 8728)
                             ("username" . "username")
                             ("password" . "password")
                             ("credentials_file" . ,%mktxp-credentials-path)
                             ,@%mktxp-router-settings))
              (map (lambda (router)
                     (match-record router <mktxp-router>
                       (name address port settings)
                       (ini-section name
                                    `(("hostname" . ,address)
                                      ("port" . ,port)
                                      ,@settings))))
                   routers)))))))))

(define (mktxp-oci-extension config)
  (match-record config <mktxp-configuration>
    (image uid gid credentials-file memory)
    (oci-extension
     (containers
      (list (oci-container-configuration
             (provision "mktxp")
             (user "root")
             (group "root")
             (image image)
             (network "host")
             (container-user (container-user uid gid))
             (respawn? #t)
             (log-file "/var/log/mktxp.log")
             (volumes
              `((,(mktxp-config-directory config)
                 . ,(string-append %mktxp-config-path ":ro"))
                (,credentials-file
                 . ,(string-append %mktxp-credentials-path ":ro"))))
             (command (list "mktxp" "--cfg-dir" %mktxp-config-path
                            "export"))
             (extra-arguments (list "--memory" memory))))))))

(define (mktxp-activation config)
  (match-record config <mktxp-configuration> (credentials-file uid)
    (secret-file-check credentials-file uid)))

(define (mktxp-prometheus-extension config)
  (match-record config <mktxp-configuration> (listen-address scrape-interval)
    (prometheus-extension
     (scrape-jobs
      (list (prometheus-scrape-job
             (name "mktxp")
             (targets (list listen-address))
             (interval scrape-interval)
             (timeout "30s")))))))

(define mktxp-service-type
  (service-type
   (name 'mktxp)
   (extensions
    (list (service-extension oci-service-type mktxp-oci-extension)
          (service-extension activation-service-type mktxp-activation)
          (service-extension prometheus-service-type
                             mktxp-prometheus-extension)))
   (description "Run mktxp in a container, exporting the interfaces, routes
and resources of MikroTik RouterOS devices through their API.")))


;;;
;;; Loki.
;;;

(define %loki-image
  "docker.io/grafana/loki:3.7.9@sha256:0db216b630ae1f348a1584c67f3cead8b4c1fc839169a39501db664744158e28")

(define-record-type* <loki-configuration>
  loki-configuration
  make-loki-configuration
  loki-configuration?
  (image              loki-configuration-image
                      (default %loki-image))
  (uid                loki-configuration-uid
                      (default 10001))
  (gid                loki-configuration-gid
                      (default 10001))
  (listen-address     loki-configuration-listen-address ;host only
                      (default "127.0.0.1"))
  (http-port          loki-configuration-http-port
                      (default 3100))
  (grpc-port          loki-configuration-grpc-port
                      (default 9095))
  (data-directory     loki-configuration-data-directory
                      (default "/var/lib/loki"))
  ;; A multiple of 24h, the period of the index.
  (retention          loki-configuration-retention
                      (default "720h")
                      (sanitize (sanitize-duration "retention")))
  ;; The first read of existing logs comes as a burst.
  (ingestion-rate-mb  loki-configuration-ingestion-rate-mb
                      (default 8))
  ;; Groups of LogQL alert rules for the ruler.
  (rule-groups        loki-configuration-rule-groups
                      (default '()))
  (alertmanager-url   loki-configuration-alertmanager-url
                      (default "http://127.0.0.1:9093"))
  (memory             loki-configuration-memory
                      (default "1g")))

(define %loki-config-path "/etc/loki/loki.yml")
;; With authentication off, everything belongs to the tenant "fake".
(define %loki-rules-path "/etc/loki/rules/fake/rules.yml")

(define (loki-config-file config)
  (match-record config <loki-configuration>
    (listen-address http-port grpc-port retention ingestion-rate-mb
                    alertmanager-url)
    (json-file
     "loki.yml"
     `(("auth_enabled" . #f)
       ("server"
        ("http_listen_address" . ,listen-address)
        ("http_listen_port" . ,http-port)
        ("grpc_listen_address" . ,listen-address)
        ("grpc_listen_port" . ,grpc-port)
        ("log_level" . "warn"))
       ;; A single instance: rings in memory, no memberlist port.
       ("common"
        ("instance_addr" . ,listen-address)
        ("path_prefix" . "/loki")
        ("replication_factor" . 1)
        ("ring" ("kvstore" ("store" . "inmemory")))
        ("storage"
         ("filesystem"
          ("chunks_directory" . "/loki/chunks")
          ("rules_directory" . "/loki/rules"))))
       ("schema_config"
        ("configs"
         . #((("from" . "2026-01-01")
              ("store" . "tsdb")
              ("object_store" . "filesystem")
              ("schema" . "v13")
              ("index" ("prefix" . "index_") ("period" . "24h"))))))
       ;; Retention is the compactor's job; without it nothing is deleted.
       ("compactor"
        ("working_directory" . "/loki/compactor")
        ("retention_enabled" . #t)
        ("delete_request_store" . "filesystem"))
       ("limits_config"
        ("retention_period" . ,retention)
        ("ingestion_rate_mb" . ,ingestion-rate-mb)
        ("ingestion_burst_size_mb" . ,(* 2 ingestion-rate-mb))
        ("reject_old_samples" . #t)
        ("reject_old_samples_max_age" . "168h"))
       ("ruler"
        ("storage" ("type" . "local")
         ("local" ("directory" . ,(dirname (dirname %loki-rules-path)))))
        ("rule_path" . "/loki/ruler")
        ("alertmanager_url" . ,alertmanager-url)
        ("ring" ("kvstore" ("store" . "inmemory")))
        ("enable_api" . #t))
       ("analytics" ("reporting_enabled" . #f))))))

(define (loki-oci-extension config)
  (match-record config <loki-configuration>
    (image uid gid data-directory rule-groups memory)
    (oci-extension
     (containers
      (list (oci-container-configuration
             (provision "loki")
             (user "root")
             (group "root")
             (image image)
             (network "host")
             (container-user (container-user uid gid))
             (respawn? #t)
             (log-file "/var/log/loki.log")
             (volumes
              `((,(loki-config-file config)
                 . ,(string-append %loki-config-path ":ro"))
                (,(alert-rule-groups-file "loki-rules.yml" rule-groups)
                 . ,(string-append %loki-rules-path ":ro"))
                (,data-directory . "/loki")))
             (command (list (string-append "-config.file="
                                           %loki-config-path)))
             (extra-arguments (list "--memory" memory))))))))

(define (loki-activation config)
  (match-record config <loki-configuration> (data-directory uid gid)
    (data-directory-activation data-directory uid gid)))

(define (loki-prometheus-extension config)
  (match-record config <loki-configuration> (listen-address http-port)
    (prometheus-extension
     (scrape-jobs
      (list (prometheus-scrape-job
             (name "loki")
             (targets (list (string-append listen-address ":"
                                           (number->string http-port))))))))))

(define loki-service-type
  (service-type
   (name 'loki)
   (extensions
    (list (service-extension oci-service-type loki-oci-extension)
          (service-extension activation-service-type loki-activation)
          (service-extension prometheus-service-type
                             loki-prometheus-extension)))
   (default-value (loki-configuration))
   (description "Run Loki in a container, as a single instance keeping logs
on the local file system, with a ruler sending LogQL alerts to
Alertmanager.")))


;;;
;;; Grafana Alloy, shipping log files to Loki.
;;;

(define %alloy-image
  "docker.io/grafana/alloy:v1.21.2@sha256:2fc2ae17ed3b712e37047724efd1cdf7a4124016c0b7522ac469dab0ddf8a8d7")

;; Log files matching PATH, a glob, get the label job=JOB.  Lines matching
;; one of the DROP regexps (RE2 syntax) are not shipped.
(define-record-type* <log-source>
  log-source
  make-log-source
  log-source?
  (path  log-source-path)                          ;string
  (job   log-source-job)                           ;string
  (drop  log-source-drop                           ;list of strings
         (default '())))

(define-record-type* <alloy-configuration>
  alloy-configuration
  make-alloy-configuration
  alloy-configuration?
  (image           alloy-configuration-image
                   (default %alloy-image))
  (listen-address  alloy-configuration-listen-address
                   (default "127.0.0.1:12345"))
  (data-directory  alloy-configuration-data-directory
                   (default "/var/lib/alloy"))
  (loki-url        alloy-configuration-loki-url
                   (default "http://127.0.0.1:3100/loki/api/v1/push"))
  ;; Read-only mounted into the container at the same path.
  (log-directory   alloy-configuration-log-directory
                   (default "/var/log"))
  (log-sources     alloy-configuration-log-sources) ;list of <log-source>
  ;; Lines starting with a "YYYY-MM-DD hh:mm:ss" time, as the system log and
  ;; the shepherd service logs write it, keep that time in this time zone.
  ;; Otherwise they would get the time they are read at, and a first read
  ;; of old logs would look like a burst of fresh events to the alerts.
  (timezone        alloy-configuration-timezone
                   (default "UTC"))
  (memory          alloy-configuration-memory
                   (default "512m")))

(define %alloy-config-path "/etc/alloy/config.alloy")

(define (alloy-string str)
  ;; Alloy strings take the escapes of JSON strings.
  (scm->json-string str))

(define (alloy-config-file config)
  (match-record config <alloy-configuration> (loki-url log-sources timezone)
    (plain-file
     "config.alloy"
     (string-append
      "local.file_match \"logs\" {\n"
      "  path_targets = [\n"
      (string-concatenate
       (map (lambda (source)
              (string-append
               "    {\"__path__\" = " (alloy-string (log-source-path source))
               ", \"job\" = " (alloy-string (log-source-job source)) "},\n"))
            log-sources))
      "  ]\n"
      "}\n\n"
      "loki.source.file \"logs\" {\n"
      "  targets    = local.file_match.logs.targets\n"
      "  forward_to = [loki.process.logs.receiver]\n"
      "}\n\n"
      "loki.process \"logs\" {\n"
      "  stage.regex {\n"
      "    expression = "
      (alloy-string "^(?P<time>\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}) ")
      "\n"
      "  }\n"
      "  stage.timestamp {\n"
      "    source   = \"time\"\n"
      "    format   = \"2006-01-02 15:04:05\"\n"
      "    location = " (alloy-string timezone) "\n"
      "  }\n"
      (string-concatenate
       (map (lambda (source)
              (if (null? (log-source-drop source))
                  ""
                  (string-append
                   "  stage.match {\n"
                   "    selector = "
                   (alloy-string (string-append
                                  "{job=\"" (log-source-job source) "\"}"))
                   "\n"
                   (string-concatenate
                    (map (lambda (regexp)
                           (string-append
                            "    stage.drop {\n"
                            "      expression = " (alloy-string regexp) "\n"
                            "    }\n"))
                         (log-source-drop source)))
                   "  }\n")))
            log-sources))
      "  forward_to = [loki.write.local.receiver]\n"
      "}\n\n"
      "loki.write \"local\" {\n"
      "  endpoint {\n"
      "    url = " (alloy-string loki-url) "\n"
      "  }\n"
      "}\n"))))

(define (alloy-oci-extension config)
  (match-record config <alloy-configuration>
    (image listen-address data-directory log-directory memory)
    (oci-extension
     (containers
      (list (oci-container-configuration
             (provision "alloy")
             (user "root")
             (group "root")
             (image image)
             (network "host")
             ;; Root, without capabilities: the owner of the log files.
             (container-user "0:0")
             (respawn? #t)
             (log-file "/var/log/alloy.log")
             (volumes
              `((,(alloy-config-file config)
                 . ,(string-append %alloy-config-path ":ro"))
                (,log-directory . ,(string-append log-directory ":ro"))
                (,data-directory . "/var/lib/alloy")))
             (command
              (list "run" %alloy-config-path
                    "--storage.path=/var/lib/alloy/data"
                    (string-append "--server.http.listen-addr="
                                   listen-address)
                    "--disable-reporting"))
             (extra-arguments
              (list "--cap-drop" "ALL" "--memory" memory))))))))

(define (alloy-activation config)
  (data-directory-activation (alloy-configuration-data-directory config)
                             0 0))

(define (alloy-prometheus-extension config)
  (prometheus-extension
   (scrape-jobs
    (list (prometheus-scrape-job
           (name "alloy")
           (targets (list (alloy-configuration-listen-address config))))))))

(define alloy-service-type
  (service-type
   (name 'alloy)
   (extensions
    (list (service-extension oci-service-type alloy-oci-extension)
          (service-extension activation-service-type alloy-activation)
          (service-extension prometheus-service-type
                             alloy-prometheus-extension)))
   (description "Run Grafana Alloy in a container, shipping log files of the
host to Loki.  Its log of the last read positions survives restarts.")))


;;;
;;; Textfile collectors for the node exporter.
;;;

;; Pass the same directory as `textfile-directory' of
;; `prometheus-node-exporter-configuration'; its default lies inside the
;; data directory of Prometheus.
(define %node-textfile-directory "/var/lib/node-exporter/textfile")

;; The default images of this module, for the update checks of
;; `node-textfile-collectors-service-type'.
(define %monitoring-images
  (list %prometheus-image %alertmanager-image %grafana-image
        %blackbox-exporter-image %mktxp-image %loki-image %alloy-image))

(define-record-type* <node-textfile-collectors-configuration>
  node-textfile-collectors-configuration
  make-node-textfile-collectors-configuration
  node-textfile-collectors-configuration?
  (directory         node-textfile-collectors-configuration-directory
                     (default %node-textfile-directory))
  ;; NVMe devices for the SMART collector, e.g. '("/dev/nvme0").
  (smart-devices     node-textfile-collectors-configuration-smart-devices
                     (default '()))
  ;; Shepherd services reported as shepherd_service_running.
  (watched-services  node-textfile-collectors-configuration-watched-services
                     (default '()))
  ;; Container images, "name:tag" with or without a digest, checked daily
  ;; for newer releases, e.g. %monitoring-images.  When there are any, the
  ;; age of the system generation is reported too.
  (update-images     node-textfile-collectors-configuration-update-images
                     (default '())))

(define %textfile-module '(johnlepikhin system services monitoring textfile))

;; The shepherd that runs the system, not the one of this Guix revision.
(define %system-herd "/run/current-system/profile/bin/herd")

(define (shepherd-collector-program config)
  (program-file
   "shepherd-textfile-collector"
   (with-extensions (list guile-json-4)
     (with-imported-modules (list %textfile-module)
       #~(begin
           (use-modules #$%textfile-module)
           (collect-shepherd
            #$(node-textfile-collectors-configuration-directory config)
            #$%system-herd
            #$(file-append coreutils "/bin/timeout")
            '#$(node-textfile-collectors-configuration-watched-services
                config)))))))

(define (smart-collector-program config)
  (program-file
   "smart-textfile-collector"
   (with-extensions (list guile-json-4)
     (with-imported-modules (list %textfile-module)
       #~(begin
           (use-modules #$%textfile-module)
           (collect-smart
            #$(node-textfile-collectors-configuration-directory config)
            #$(file-append smartmontools "/sbin/smartctl")
            #$(file-append coreutils "/bin/timeout")
            '#$(node-textfile-collectors-configuration-smart-devices
                config)))))))

(define (updates-collector-program config)
  (program-file
   "updates-textfile-collector"
   (with-extensions (list guile-json-4)
     (with-imported-modules (list %textfile-module)
       #~(begin
           (use-modules #$%textfile-module)
           (collect-updates
            #$(node-textfile-collectors-configuration-directory config)
            #$(file-append skopeo "/bin/skopeo")
            #$(file-append coreutils "/bin/timeout")
            '#$(node-textfile-collectors-configuration-update-images config)
            "/var/guix/profiles/system"))))))

;; Collection interval of the shepherd collector, in seconds;
;; `service-alert-rules' takes it as stale after three of them.  The others
;; publish their own textfile_collector_max_age_seconds.
(define %shepherd-collector-interval 60)

(define (node-textfile-collectors-mcron-jobs config)
  (append
   (if (null? (node-textfile-collectors-configuration-watched-services
               config))
       '()
       (list #~(job "* * * * *" #$(shepherd-collector-program config))))
   (if (null? (node-textfile-collectors-configuration-smart-devices config))
       '()
       (list #~(job "*/5 * * * *" #$(smart-collector-program config))))
   (if (null? (node-textfile-collectors-configuration-update-images config))
       '()
       (list #~(job "0 9 * * *" #$(updates-collector-program config))))))

(define (node-textfile-collectors-activation config)
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p #$(node-textfile-collectors-configuration-directory config))))

(define node-textfile-collectors-service-type
  (service-type
   (name 'node-textfile-collectors)
   (extensions
    (list (service-extension mcron-service-type
                             node-textfile-collectors-mcron-jobs)
          (service-extension activation-service-type
                             node-textfile-collectors-activation)))
   (default-value (node-textfile-collectors-configuration))
   (description "Write the state of shepherd services, the SMART health of
NVMe drives and the available updates of container images to the textfile
directory of the Prometheus node exporter.")))


;;;
;;; Rule libraries.
;;;

(define* (host-alert-rules #:key
                           (cpu-temperature 95)
                           (cpu-sensor-label "Tctl")
                           (filesystem-selector "mountpoint=\"/\"")
                           (disk-free-warning 150e9)
                           (disk-free-critical 50e9)
                           (disk-fill-guard 400e9)
                           (memory-available-ratio 0.05)
                           (network-links '()))
  "Return alert groups for the hardware and resources of a host watched by
the node exporter and the SMART collector.  Byte amounts are numbers.
NETWORK-LINKS is an alist of interface names to their expected speed in
Mbit/s."
  (let ((free (string-append "node_filesystem_avail_bytes{"
                             filesystem-selector "}"))
        (n number->config-string))
    (list
     (alert-rule-group
      (name "hardware")
      (rules
       (append
        (list
         (alert-rule
          (alert "CpuHot")
          (expr (string-append
                 "max by (instance) (node_hwmon_temp_celsius"
                 " * on (chip, sensor) group_left"
                 " node_hwmon_sensor_label{label=\"" cpu-sensor-label "\"})"
                 " >= " (n cpu-temperature)))
          (for "5m")
          (category "hardware")
          (summary "CPU at {{ $value }} °C"))
         (alert-rule
          (alert "NvmeHot")
          (expr "smart_temperature_celsius \
>= smart_warning_temperature_celsius")
          (for "10m")
          (category "hardware")
          (summary "{{ $labels.device }} at {{ $value }} °C, above its warning \
temperature"))
         (alert-rule
          (alert "NvmeCriticalWarning")
          (expr "smart_critical_warning != 0")
          (severity 'critical)
          (category "hardware")
          (summary "{{ $labels.device }} reports critical warning \
{{ $value }}: check smartctl -a and the backups"))
         (alert-rule
          (alert "NvmeWearing")
          (expr "smart_percentage_used >= 80 or smart_available_spare_ratio \
<= smart_available_spare_threshold_ratio + 0.1")
          (category "hardware")
          (summary "{{ $labels.device }} is wearing out"))
         (alert-rule
          (alert "NvmeMediaErrors")
          (expr "increase(smart_media_errors_total[1h]) > 0")
          (severity 'critical)
          (category "hardware")
          (summary "{{ $labels.device }} has new media errors: check \
smartctl -a and the backups")))
        (map (match-lambda
               ((device . mbits)
                (alert-rule
                 (alert "LinkDegraded")
                 (expr (string-append
                        "node_network_carrier{device=\"" device "\"} == 0"
                        " or node_network_speed_bytes{device=\"" device
                        "\"} < " (n (* mbits 125000))))
                 (for "5m")
                 (category "hardware")
                 (summary (string-append device " is down or slower than "
                                         (n mbits) " Mbit/s")))))
             network-links))))
     (alert-rule-group
      (name "resources")
      (rules
       (list
        (alert-rule
         (alert "DiskLow")
         (expr (string-append free " < " (n disk-free-warning)))
         (for "30m")
         (category "resources")
         (summary "{{ $labels.mountpoint }}: {{ $value | humanize1024 }}B \
free"))
        (alert-rule
         (alert "DiskCritical")
         (expr (string-append free " < " (n disk-free-critical)))
         (for "5m")
         (severity 'critical)
         (category "resources")
         (summary "{{ $labels.mountpoint }}: only {{ $value | humanize1024 \
}}B free"))
        (alert-rule
         (alert "DiskFillingFast")
         (expr (string-append "predict_linear(" free "[6h], 24 * 3600) < 0"
                              " and " free " < " (n disk-fill-guard)))
         (for "1h")
         (category "resources")
         (summary "{{ $labels.mountpoint }} fills up within a day"))
        (alert-rule
         (alert "FilesystemReadOnly")
         (expr (string-append "node_filesystem_readonly{"
                              filesystem-selector "} == 1"))
         (for "1m")
         (severity 'critical)
         (category "resources")
         (summary "{{ $labels.mountpoint }} is read-only: check the kernel \
log"))
        (alert-rule
         (alert "MemoryPressure")
         (expr (string-append "node_memory_MemAvailable_bytes"
                              " / node_memory_MemTotal_bytes < "
                              (n memory-available-ratio)))
         (for "10m")
         (category "resources")
         (summary "Available memory is low"))
        (alert-rule
         (alert "ClockUnsynced")
         (expr "node_timex_sync_status == 0")
         (for "30m")
         (category "resources")
         (summary "The clock is not synchronized"))
        (alert-rule
         (alert "HostRebooted")
         (expr "time() - node_boot_time_seconds < 1800")
         (category "resources")
         (summary "The host booted {{ $value | humanizeDuration }} ago"))))))))

(define* (service-alert-rules #:key
                              (runner-errors 3))
  "Return alert groups for services: shepherd services, scrape targets,
blackbox probes other than uplink probes, the textfile collectors and
GitLab Runner.  RUNNER-ERRORS is how many runner errors within 15 minutes
make an alert."
  (list
   (alert-rule-group
    (name "services")
    (rules
     (list
      (alert-rule
       (alert "ServiceDown")
       ;; Only trusted while the collector itself works.
       (expr "shepherd_service_running == 0 and on () \
textfile_collector_success{collector=\"shepherd\"} == 1")
       (for "5m")
       (severity 'critical)
       (category "services")
       (summary "Service {{ $labels.service }} is not running"))
      (alert-rule
       (alert "ProbeFailed")
       ;; Uplink probes have their own rules; presence probes of machines
       ;; that come and go are no failure.
       (expr "probe_success{uplink=\"\",presence=\"\"} == 0")
       (for "3m")
       (severity 'critical)
       (category "services")
       (summary "{{ $labels.job }}: {{ $labels.instance }} does not answer"))
      (alert-rule
       (alert "TargetDown")
       (expr "up == 0")
       (for "5m")
       (category "monitoring")
       (summary "Scrape target {{ $labels.job }} ({{ $labels.instance }}) \
is down"))
      (alert-rule
       (alert "ShepherdCollectorFailing")
       (expr (string-append
              "textfile_collector_success{collector=\"shepherd\"} == 0"
              " or time() - textfile_collector_last_run_timestamp_seconds"
              "{collector=\"shepherd\"} > "
              (number->string (* 3 %shepherd-collector-interval))
              " or absent(textfile_collector_last_run_timestamp_seconds"
              "{collector=\"shepherd\"})"))
       (for "5m")
       (severity 'critical)
       (category "monitoring")
       (summary "Service states are not collected: check mcron"))
      (alert-rule
       (alert "TextfileCollectorFailing")
       ;; Each collector publishes how old its output may get.
       (expr "textfile_collector_success{collector!=\"shepherd\"} == 0 \
or (time() - textfile_collector_last_run_timestamp_seconds\
{collector!=\"shepherd\"}) > on (instance, collector) \
textfile_collector_max_age_seconds or node_textfile_scrape_error == 1")
       (for "10m")
       (category "monitoring")
       (summary "Textfile collector {{ $labels.collector }} fails"))
      (alert-rule
       (alert "CertificateExpiring")
       (expr "probe_ssl_earliest_cert_expiry - time() < 60 * 86400")
       (for "1h")
       (category "services")
       (summary "Certificate of {{ $labels.instance }} expires in \
{{ $value | humanizeDuration }}"))
      (alert-rule
       (alert "RunnerErrors")
       (expr (string-append
              "sum by (instance) (increase(gitlab_runner_errors_total"
              "{level=~\"error|fatal|panic\"}[15m])) > "
              (number->string runner-errors)))
       (category "services")
       (summary "GitLab Runner logs errors: see \
/var/log/gitlab-runner.log")))))))

(define* (stack-alert-rules #:key (prometheus-retention-bytes 20e9))
  "Return alert groups about the monitoring itself.  The always-firing
Watchdog lets the receiver notice when alerts stop arriving.
PROMETHEUS-RETENTION-BYTES is the retention size of Prometheus."
  (list
   (alert-rule-group
    (name "monitoring")
    (rules
     (list
      (alert-rule
       (alert "Watchdog")
       (expr "vector(1)")
       (severity 'info)
       (summary "The alerting pipeline works"))
      (alert-rule
       (alert "RuleEvaluationFailing")
       (expr "increase({__name__=~\"(loki_)?prometheus_rule_evaluation_\
failures_total\"}[15m]) > 0")
       (category "monitoring")
       (summary "Alert rules fail to evaluate in {{ $labels.job }}"))
      (alert-rule
       (alert "NotificationsDropped")
       (expr "increase(prometheus_notifications_dropped_total[15m]) > 0 \
or prometheus_notifications_alertmanagers_discovered < 1")
       (for "5m")
       (category "monitoring")
       (summary "Prometheus cannot hand alerts to Alertmanager"))
      (alert-rule
       (alert "AlertDeliveryFailing")
       (expr "increase(alertmanager_notifications_failed_total[15m]) > 0")
       (category "monitoring")
       (summary "Alertmanager fails to deliver notifications"))
      (alert-rule
       (alert "PrometheusStorage")
       (expr (string-append
              "increase(prometheus_tsdb_compactions_failed_total[3h]) > 0"
              " or (prometheus_tsdb_storage_blocks_bytes"
              " + prometheus_tsdb_wal_storage_size_bytes"
              " + prometheus_tsdb_head_chunks_storage_size_bytes) > "
              (number->config-string (* 0.9 prometheus-retention-bytes))))
       (category "monitoring")
       (summary "Prometheus storage fails or nears its size limit")))))))

(define* (network-alert-rules #:key
                              (uplinks '())
                              (router-ports '())
                              (latency-threshold 0.2)
                              (router-cpu-load 90)
                              (router-free-memory 10e6))
  "Return alert groups for a MikroTik router and its uplinks, watched by
mktxp and by blackbox ICMP probes labelled uplink=NAME.  UPLINKS is a list
of (NAME INTERFACE) for the uplinks and their router interfaces: a probe
alone cannot tell a dead uplink, as the router sends the probes of a dead
one through the others.  ROUTER-PORTS is an alist of router interfaces to
their expected rate in Mbit/s.  LATENCY-THRESHOLD is in seconds."
  (let ((n number->config-string))
    (list
     (alert-rule-group
      (name "network")
      (rules
       (append
        (map (match-lambda
               ((uplink interface)
                (alert-rule
                 (alert "UplinkDown")
                 (expr (string-append
                        "max(mktxp_interface_running{name=\"" interface
                        "\"}) == 0 or max(probe_success{uplink=\"" uplink
                        "\"}) == 0"))
                 (for "3m")
                 (category "network")
                 (labels `(("uplink" . ,uplink)))
                 (summary (string-append "Uplink " uplink " (" interface
                                         ") is down")))))
             uplinks)
        (map (match-lambda
               ((uplink interface)
                (alert-rule
                 (alert "UplinkLatencyHigh")
                 (expr (string-append
                        "quantile_over_time(0.5, probe_icmp_duration_seconds"
                        "{uplink=\"" uplink "\",phase=\"rtt\"}[15m]) > "
                        (n latency-threshold)))
                 (for "5m")
                 (category "network")
                 (labels `(("uplink" . ,uplink)))
                 (summary (string-append
                           "Uplink " uplink ": median RTT {{ $value | "
                           "humanizeDuration }}")))))
             uplinks)
        (if (null? uplinks)
            '()
            (list (alert-rule
                   (alert "UplinksAllDown")
                   (expr (string-append
                          "count(ALERTS{alertname=\"UplinkDown\","
                          "alertstate=\"firing\"}) >= "
                          (n (length uplinks))))
                   (severity 'critical)
                   (category "network")
                   (summary "All uplinks were down"))))
        (map (match-lambda
               ((interface . mbits)
                (alert-rule
                 (alert "RouterPortSlow")
                 (expr (string-append
                        "mktxp_interface_rate{name=\"" interface "\"} < "
                        (n mbits)))
                 (for "10m")
                 (category "network")
                 (summary (string-append
                           interface " runs at {{ $value }} Mbit/s instead of "
                           (n mbits))))))
             router-ports)
        (list
         (alert-rule
          (alert "RouterCpuHigh")
          (expr (string-append "mktxp_system_cpu_load > "
                               (n router-cpu-load)))
          (for "10m")
          (category "network")
          (summary "Router {{ $labels.routerboard_name }}: CPU at \
{{ $value }} %"))
         (alert-rule
          (alert "RouterMemoryLow")
          (expr (string-append "mktxp_system_free_memory < "
                               (n router-free-memory)))
          (for "10m")
          (category "network")
          (summary "Router {{ $labels.routerboard_name }}: \
{{ $value | humanize1024 }}B of memory free")))))))))

(define* (log-alert-rules #:key (job "system") (kernel-tag "linux"))
  "Return LogQL alert groups for the Loki ruler about the system log, whose
lines have the label job=JOB; kernel messages are tagged KERNEL-TAG there."
  (let ((kernel (string-append "{job=\"" job "\"} |= \" " kernel-tag ": \"")))
    (list
     (alert-rule-group
      (name "logs")
      (rules
       (list
        (alert-rule
         (alert "KernelHardwareError")
         (expr (string-append
                "sum(count_over_time(" kernel
                " |~ `nvme[0-9]+.*(timeout|reset|error)|I/O error, dev"
                "|mce: |Hardware Error|EXT4-fs error|BUG: |Oops`"
                " [10m])) > 0"))
         (severity 'critical)
         (category "hardware")
         (summary "The kernel reports hardware or file system errors: see \
/var/log/messages"))
        (alert-rule
         (alert "OomKill")
         (expr (string-append
                "sum(count_over_time(" kernel
                " |= `Out of memory: Killed process` [10m])) > 0"))
         (category "resources")
         (summary "The kernel OOM killer killed a process: earlyoom did not \
prevent it"))
        (alert-rule
         (alert "ServiceDisabled")
         ;; Transient services, such as the sshd of each connection, are
         ;; disabled whenever they end.
         (expr (string-append
                "sum by (service) (count_over_time({job=\"" job "\"}"
                " |~ `shepherd\\[1\\]: Service [^ ]+ has been disabled`"
                " !~ `Service sshd-[0-9]+ `"
                " | regexp `Service (?P<service>[^ ]+) has been disabled`"
                " [10m])) > 0"))
         (category "services")
         (summary "Shepherd disabled {{ $labels.service }}, which kept \
failing"))))))))

;; A repository kept by `restic-maintenance-service-type', for
;; `backup-alert-rules'.
(define-record-type* <backup-repository>
  backup-repository
  make-backup-repository
  backup-repository?
  (name          backup-repository-name)          ;"laptop/home"
  ;; Its newest snapshot counts as stale after this long...
  (max-age       backup-repository-max-age
                 (default "2d")
                 (sanitize (sanitize-duration "max-age")))
  ;; ...but for a machine that is often away, only if this PromQL selector
  ;; of a presence probe, such as probe_success{presence="laptop"}, saw it
  ;; between MAX-AGE and half of it ago: back long enough to have made a
  ;; backup.  #f for a machine that is always there.
  (presence      backup-repository-presence
                 (default #f))
  ;; Stale regardless of presence.
  (away-max-age  backup-repository-away-max-age
                 (default "30d")
                 (sanitize (sanitize-duration "away-max-age"))))

(define (backup-stale-rule repository)
  (match-record repository <backup-repository>
    (name max-age presence away-max-age)
    (let* ((n number->config-string)
           (mtime (string-append
                   "max(restic_repository_snapshot_mtime_seconds"
                   "{repository=\"" name "\"})"))
           (age (lambda (limit)
                  (string-append "time() - " mtime " > "
                                 (n (duration->seconds limit)))))
           (half (n (quotient (duration->seconds max-age) 2))))
      (alert-rule
       (alert "BackupStale")
       (expr
        (string-append
         (if presence
             (string-append
              "(" (age max-age) " and on () max_over_time("
              presence "[" half "s] offset " half "s) > 0) or "
              (age away-max-age))
             (age max-age))
         ;; absent() catches a repository that never got a snapshot, or
         ;; whose directory went away.
         " or absent(restic_repository_snapshot_mtime_seconds"
         "{repository=\"" name "\"})"))
       (for "30m")
       (category "backups")
       (labels `(("repository" . ,name)))
       (summary (string-append "No new backup in " name
                               " for {{ $value | humanizeDuration }}"))))))

(define* (backup-alert-rules #:key (repositories '()) (storage-bytes #f))
  "Return alert groups for restic repositories kept by
`restic-maintenance-service-type'.  REPOSITORIES is a list of
<backup-repository> records.  STORAGE-BYTES, when a number, bounds the size
of all of them together."
  (let ((n number->config-string)
        (stale-rules (map backup-stale-rule repositories)))
    (list
     (alert-rule-group
      (name "backups")
      (rules
       (append
        stale-rules
        (list
         (alert-rule
          (alert "BackupFutureSnapshots")
          (expr "restic_repository_future_snapshots > 0")
          (severity 'critical)
          (category "backups")
          (summary "{{ $labels.repository }} has {{ $value }} snapshots \
dated in the future: forgetting is stopped; check the client's clock or a \
compromised client"))
         (alert-rule
          (alert "BackupMaintenanceFailing")
          (expr "restic_maintenance_success == 0")
          (category "backups")
          (summary "Backup maintenance of {{ $labels.repository }} fails at \
{{ $labels.step }}: see /var/log/mcron.log")))
        (if storage-bytes
            (list (alert-rule
                   (alert "BackupStorageLarge")
                   (expr (string-append
                          "sum(restic_repository_size_bytes) > "
                          (n storage-bytes)))
                   (category "backups")
                   (summary "Backups take {{ $value | humanize1024 }}B")))
            '())))))))

(define (duration->seconds duration)
  "Seconds in DURATION, a Prometheus duration such as \"2d\" or \"36h\"."
  (let loop ((parts (map (lambda (m)
                           (cons (string->number (match:substring m 1))
                                 (match:substring m 2)))
                         (list-matches "([0-9]+)(ms|s|m|h|d|w|y)" duration)))
             (total 0))
    (match parts
      (() total)
      (((count . unit) . rest)
       (loop rest
             (+ total
                (* count
                   (assoc-ref '(("ms" . 1/1000) ("s" . 1) ("m" . 60)
                                ("h" . 3600) ("d" . 86400) ("w" . 604800)
                                ("y" . 31536000))
                              unit))))))))

(define* (home-assistant-alert-rules #:key
                                     (battery-percent 20)
                                     (device-entities ".*")
                                     (unavailable-for "1h")
                                     (outage #f))
  "Return alert groups for the devices of Home Assistant, from the metrics of
its prometheus integration.  DEVICE-ENTITIES is a regular expression of the
entities, one per device, whose unavailability means the device is gone,
e.g. its battery or power sensor.  OUTAGE, when a PromQL expression, is true
during a power cut, which takes mains devices away as expected."
  (let ((n number->config-string))
    (list
     (alert-rule-group
      (name "home-assistant")
      (rules
       (list
        (alert-rule
         (alert "DeviceBatteryLow")
         (expr (string-append "hass_sensor_battery_percent < "
                              (n battery-percent)))
         (for "1h")
         (category "devices")
         (summary "Battery of {{ $labels.friendly_name }} \
({{ $labels.entity }}) is at {{ $value }}%"))
        (alert-rule
         (alert "DeviceUnavailable")
         (expr (string-append
                ;; A raw string: the regular expression goes as it is.
                "hass_entity_available{entity=~`" device-entities "`} == 0"
                (if outage
                    (string-append " unless on () (" outage ")")
                    "")))
         (for unavailable-for)
         (category "devices")
         (summary "{{ $labels.entity }} has been unavailable for a while: \
the device is off, out of range or out of battery"))))))))

(define* (update-alert-rules #:key (system-max-age "30d"))
  "Return alert groups for the update checks of the textfile collectors.
The alerts carry notify=\"weekly\", for an Alertmanager route that repeats
them rarely and in one notification.  SYSTEM-MAX-AGE is how long a system
generation may stay before it counts as outdated."
  (let ((n number->config-string)
        (weekly '(("notify" . "weekly"))))
    (list
     (alert-rule-group
      (name "updates")
      (rules
       (list
        (alert-rule
         (alert "ContainerImageUpdateAvailable")
         (expr "container_image_update_available == 1")
         (for "1h")
         (severity 'info)
         (labels weekly)
         (summary "{{ $labels.image }}: {{ $labels.latest }} is out, \
{{ $labels.current }} runs"))
        (alert-rule
         (alert "SystemOutdated")
         (expr (string-append
                "time() - guix_system_generation_timestamp_seconds > "
                (n (duration->seconds system-max-age))))
         (for "1h")
         (severity 'info)
         (labels weekly)
         (summary "The system was last deployed \
{{ $value | humanizeDuration }} ago"))
        (alert-rule
         (alert "UpdateCheckFailing")
         (expr "container_image_check_success == 0")
         (for "2d")
         (severity 'info)
         (labels weekly)
         (summary "Tags of {{ $labels.image }} cannot be listed"))))))))

(define* (prebuild-alert-rules #:key (max-age "2d") (publish-cache-bytes #f))
  "Return alert groups for `guix-prebuild-service-type'.  MAX-AGE is how
long the newest published run may get; PUBLISH-CACHE-BYTES, when a number,
bounds the guix publish cache."
  (let ((n number->config-string))
    (list
     (alert-rule-group
      (name "prebuild")
      (rules
       (append
        (list
         (alert-rule
          (alert "PrebuildFailing")
          (expr (string-append "guix_prebuild_step_success == 0 or "
                               "guix_prebuild_target_success == 0"))
          (category "builds")
          (summary "Prebuild fails at {{ $labels.step }}{{ $labels.target }}: \
see /var/log/guix-prebuild"))
         (alert-rule
          (alert "PrebuildStale")
          ;; absent() also covers a host where no run was ever published.
          (expr (string-append
                 "time() - guix_prebuild_last_success_timestamp_seconds > "
                 (n (duration->seconds max-age))
                 " or absent(guix_prebuild_last_success_timestamp_seconds)"))
          (for "1h")
          (category "builds")
          (summary "No prebuild was published for {{ $value | \
humanizeDuration }}: updates fetch from the internet"))
         (alert-rule
          (alert "PrebuildCacheCold")
          (expr "guix_prebuild_warm_missing > 0")
          (for "1h")
          (category "builds")
          (summary "{{ $value }} items of the prebuild are not in the publish \
cache yet")))
        (if publish-cache-bytes
            (list (alert-rule
                   (alert "PublishCacheLarge")
                   (expr (string-append "guix_publish_cache_bytes > "
                                        (n publish-cache-bytes)))
                   (category "builds")
                   (summary "The guix publish cache takes \
{{ $value | humanize1024 }}B")))
            '())))))))
