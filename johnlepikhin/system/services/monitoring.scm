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

            alertmanager-configuration
            alertmanager-configuration?
            alertmanager-service-type

            grafana-datasource
            grafana-datasource?
            grafana-dashboard
            grafana-dashboard?
            %grafana-node-exporter-dashboard
            grafana-configuration
            grafana-configuration?
            grafana-service-type

            blackbox-exporter-configuration
            blackbox-exporter-configuration?
            blackbox-exporter-service-type

            %node-textfile-directory
            node-textfile-collectors-configuration
            node-textfile-collectors-configuration?
            node-textfile-collectors-service-type

            host-alert-rules
            service-alert-rules
            stack-alert-rules))


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

(define (data-directory-activation directory uid gid)
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p #$directory)
      (chown #$directory #$uid #$gid)
      (chmod #$directory #o700)))

(define (secret-file-check file uid)
  "Warn when the secret FILE is missing or readable by others than UID."
  #~(let ((file #$file))
      (cond ((not (file-exists? file))
             (format (current-error-port)
                     "warning: secret file ~a is missing; its service will \
not start~%" file))
            ((let ((st (stat file)))
               (or (not (= (stat:uid st) #$uid))
                   (not (zero? (logand (stat:perms st) #o077)))))
             (format (current-error-port)
                     "warning: secret file ~a must belong to uid ~a and have \
mode 0400~%" file #$uid)))))

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
                    (default '())))

(define (scrape-job->json job)
  (match-record job <prometheus-scrape-job>
    (name targets metrics-path params interval timeout labels sample-limit
          relabel-configs)
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
              `(("relabel_configs" . ,(list->vector relabel-configs))))))))

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
                (,data-directory . "/prometheus")))
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
  (match-record config <prometheus-configuration> (data-directory uid gid)
    (data-directory-activation data-directory uid gid)))

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
  (memory                    alertmanager-configuration-memory
                             (default "256m")))

(define %alertmanager-config-path "/etc/alertmanager/alertmanager.yml")
(define %alertmanager-webhook-url-path "/run/secrets/webhook-url")

(define (alertmanager-config-file config)
  (match-record config <alertmanager-configuration>
    (group-by group-wait group-interval repeat-interval-critical
              repeat-interval-warning repeat-interval-info inhibit-rules)
    (json-file
     "alertmanager.yml"
     `(("route"
        ("receiver" . "webhook")
        ("group_by" . ,(list->vector group-by))
        ("group_wait" . ,group-wait)
        ("group_interval" . ,group-interval)
        ("repeat_interval" . ,repeat-interval-warning)
        ("routes"
         . #((("matchers" . #("severity=\"info\""))
              ("group_wait" . "0s")
              ("repeat_interval" . ,repeat-interval-info))
             (("matchers" . #("severity=\"critical\""))
              ("group_wait" . "10s")
              ("repeat_interval" . ,repeat-interval-critical)))))
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
  (name  grafana-dashboard-name)                   ;file name, "x.json"
  (file  grafana-dashboard-file))                  ;file-like

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
  (memory               grafana-configuration-memory
                        (default "512m")))

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
                      (grafana-dashboard-file dashboard)))
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
                   (default "128m")))

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
         ("icmp" ("preferred_ip_protocol" . "ip4"))))))))

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
                 . ,(string-append %blackbox-exporter-config-path ":ro"))))
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
;;; Textfile collectors for the node exporter.
;;;

;; Pass the same directory as `textfile-directory' of
;; `prometheus-node-exporter-configuration'; its default lies inside the
;; data directory of Prometheus.
(define %node-textfile-directory "/var/lib/node-exporter/textfile")

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

;; Collection intervals, in seconds; `service-alert-rules' takes a
;; collector as stale after three of them.
(define %shepherd-collector-interval 60)
(define %smart-collector-interval 300)

(define (node-textfile-collectors-mcron-jobs config)
  (append
   (if (null? (node-textfile-collectors-configuration-watched-services
               config))
       '()
       (list #~(job "* * * * *" #$(shepherd-collector-program config))))
   (if (null? (node-textfile-collectors-configuration-smart-devices config))
       '()
       (list #~(job "*/5 * * * *" #$(smart-collector-program config))))))

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
   (description "Write the state of shepherd services and the SMART health
of NVMe drives to the textfile directory of the Prometheus node exporter.")))


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
       (expr "probe_success{uplink=\"\"} == 0")
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
       (expr (string-append
              "textfile_collector_success{collector!=\"shepherd\"} == 0"
              " or time() - textfile_collector_last_run_timestamp_seconds"
              "{collector!=\"shepherd\"} > "
              (number->string (* 3 %smart-collector-interval))
              " or node_textfile_scrape_error == 1"))
       (for "10m")
       (category "monitoring")
       (summary "Textfile collector {{ $labels.collector }} fails"))
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
