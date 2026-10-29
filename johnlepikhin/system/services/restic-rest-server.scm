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

;;; A backup server for restic clients.
;;;
;;; rest-server takes the backups over HTTPS in append-only mode: a client,
;;; even a compromised one, can add snapshots but neither delete nor
;;; overwrite them, and with private repositories each user reaches only
;;; the repositories under its own name.  Removing old snapshots is the job
;;; of the maintenance service, which runs on this host with a separate
;;; restic key of each repository.
;;;
;;; Both work on the same directory as the same user; restic's locks keep
;;; them apart.  The repository data stays encrypted by the clients' keys;
;;; whoever can read the maintenance keys, root included, can read it.

(define-module (johnlepikhin system services restic-rest-server)
  #:use-module (gnu packages admin)                ; shadow
  #:use-module (gnu packages backup)               ; restic, restic-rest-server
  #:use-module (gnu packages base)                 ; coreutils
  #:use-module (gnu packages guile)                ; guile-json-4
  #:use-module (gnu services)
  #:use-module (gnu services mcron)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (johnlepikhin system services monitoring)
  #:use-module (johnlepikhin system services utils)
  #:use-module (srfi srfi-1)
  #:export (restic-rest-server-configuration
            restic-rest-server-configuration?
            restic-rest-server-service-type

            restic-maintenance-job
            restic-maintenance-job?
            restic-maintenance-configuration
            restic-maintenance-configuration?
            restic-maintenance-service-type))


;;;
;;; rest-server.
;;;

(define-record-type* <restic-rest-server-configuration>
  restic-rest-server-configuration
  make-restic-rest-server-configuration
  restic-rest-server-configuration?
  (package          restic-rest-server-configuration-package
                    (default restic-rest-server))
  (user             restic-rest-server-configuration-user
                    (default "restic"))
  (group            restic-rest-server-configuration-group
                    (default "restic"))
  ;; Holds a directory per user, with that user's repositories in it.
  (data-directory   restic-rest-server-configuration-data-directory
                    (default "/srv/backup/restic"))
  (listen-address   restic-rest-server-configuration-listen-address
                    (default "0.0.0.0:8000"))
  ;; Secret file with bcrypt entries (`htpasswd -B'), one per client, plus
  ;; "metrics" for the Prometheus endpoint; owned by the user, mode 0400.
  (htpasswd-file    restic-rest-server-configuration-htpasswd-file
                    (default "/etc/restic-rest-server/htpasswd"))
  ;; File-like certificate, which the clients pin with --cacert.
  (tls-certificate  restic-rest-server-configuration-tls-certificate)
  ;; Secret key of the certificate, owned by the user, mode 0400.
  (tls-key-file     restic-rest-server-configuration-tls-key-file
                    (default "/etc/restic-rest-server/tls.key"))
  ;; Serve /metrics, behind the "metrics" user of the htpasswd file.
  (prometheus?      restic-rest-server-configuration-prometheus?
                    (default #t)))

(define (restic-rest-server-accounts config)
  (match-record config <restic-rest-server-configuration> (user group)
    (list (user-group (name group) (system? #t))
          (user-account
           (name user)
           (group group)
           (system? #t)
           (comment "restic backup server")
           (home-directory "/var/empty")
           (shell (file-append shadow "/sbin/nologin"))))))

(define (restic-rest-server-activation config)
  (match-record config <restic-rest-server-configuration>
    (user group data-directory htpasswd-file tls-key-file)
    #~(begin
        #$(data-directory-activation data-directory user group)
        #$(secret-file-check htpasswd-file user)
        #$(secret-file-check tls-key-file user))))

(define (restic-rest-server-shepherd-service config)
  (match-record config <restic-rest-server-configuration>
    (package user group data-directory listen-address htpasswd-file
             tls-certificate tls-key-file prometheus?)
    (list (shepherd-service
           (provision '(restic-rest-server))
           (requirement '(user-processes networking file-systems))
           (documentation "Serve restic repositories, append-only.")
           (start #~(make-forkexec-constructor
                     (list #$(file-append package "/bin/restic-rest-server")
                           "--path" #$data-directory
                           "--listen" #$listen-address
                           "--append-only"
                           "--private-repos"
                           "--htpasswd-file" #$htpasswd-file
                           "--tls"
                           "--tls-cert" #$tls-certificate
                           "--tls-key" #$tls-key-file
                           #$@(if prometheus? '("--prometheus") '()))
                     #:user #$user
                     #:group #$group
                     #:log-file "/var/log/restic-rest-server.log"))
           (stop #~(make-kill-destructor))))))

(define restic-rest-server-service-type
  (service-type
   (name 'restic-rest-server)
   (extensions
    (list (service-extension account-service-type
                             restic-rest-server-accounts)
          (service-extension activation-service-type
                             restic-rest-server-activation)
          (service-extension shepherd-root-service-type
                             restic-rest-server-shepherd-service)))
   (description "Run rest-server for restic clients over HTTPS, in
append-only mode with a private set of repositories per user.")))


;;;
;;; Maintenance.
;;;

(define (restic-duration? value)
  (and (string? value)
       (string-match "^([0-9]+[ymdh])+$" value)))

(define (sanitize-restic-duration field)
  (lambda (value)
    (unless (restic-duration? value)
      (raise (formatted-message (G_ "~a: invalid restic duration: ~s")
                                field value)))
    value))

;; Retention follows the advice of restic for append-only repositories:
;; only --keep-within* policies, which a client cannot game by adding
;; snapshots.  They count back from the newest snapshot, which is why
;; snapshots dated in the future stop the forgetting altogether.
(define-record-type* <restic-maintenance-job>
  restic-maintenance-job
  make-restic-maintenance-job
  restic-maintenance-job?
  ;; Path relative to the data directory, e.g. "laptop/home".
  (repository          restic-maintenance-job-repository)
  ;; Secret file with a restic key of the repository added for
  ;; maintenance (`restic key add'), owned by the user, mode 0400.
  (key-file            restic-maintenance-job-key-file)
  (keep-within         restic-maintenance-job-keep-within
                       (default "30d")
                       (sanitize (sanitize-restic-duration "keep-within")))
  (keep-within-weekly  restic-maintenance-job-keep-within-weekly
                       (default "6m")
                       (sanitize (sanitize-restic-duration
                                  "keep-within-weekly")))
  (keep-within-monthly restic-maintenance-job-keep-within-monthly
                       (default "2y")
                       (sanitize (sanitize-restic-duration
                                  "keep-within-monthly")))
  (keep-within-yearly  restic-maintenance-job-keep-within-yearly
                       (default "5y")
                       (sanitize (sanitize-restic-duration
                                  "keep-within-yearly")))
  ;; Snapshots dated more than this many seconds ahead are taken as forged
  ;; or as a broken clock: nothing is forgotten until they are dealt with.
  (max-future          restic-maintenance-job-max-future
                       (default 86400))
  ;; More snapshots than this means that forgetting does not work or that
  ;; a client floods the repository; nothing is forgotten either.
  (max-snapshots       restic-maintenance-job-max-snapshots
                       (default 500))
  ;; Repacking per run is bounded; the rest waits for the next day.
  (max-repack-size     restic-maintenance-job-max-repack-size
                       (default "20G")))

(define-record-type* <restic-maintenance-configuration>
  restic-maintenance-configuration
  make-restic-maintenance-configuration
  restic-maintenance-configuration?
  (restic              restic-maintenance-configuration-restic
                       (default restic))
  ;; The owner of the repositories, as for rest-server.
  (user                restic-maintenance-configuration-user
                       (default "restic"))
  (group               restic-maintenance-configuration-group
                       (default "restic"))
  (data-directory      restic-maintenance-configuration-data-directory
                       (default "/srv/backup/restic"))
  (cache-directory     restic-maintenance-configuration-cache-directory
                       (default "/var/cache/restic-maintenance"))
  (jobs                restic-maintenance-configuration-jobs) ;list of jobs
  (schedule            restic-maintenance-configuration-schedule
                       (default "30 3 * * *"))
  ;; The textfile directory of the node exporter.
  (textfile-directory  restic-maintenance-configuration-textfile-directory
                       (default %node-textfile-directory)))

(define (job->alist job)
  (match-record job <restic-maintenance-job>
    (repository key-file keep-within keep-within-weekly keep-within-monthly
                keep-within-yearly max-future max-snapshots max-repack-size)
    `((repository . ,repository)
      (key-file . ,key-file)
      (keep-flags . ("--keep-within" ,keep-within
                     "--keep-within-weekly" ,keep-within-weekly
                     "--keep-within-monthly" ,keep-within-monthly
                     "--keep-within-yearly" ,keep-within-yearly))
      (max-future . ,max-future)
      (max-snapshots . ,max-snapshots)
      (max-repack-size . ,max-repack-size))))

(define %maintenance-modules
  '((johnlepikhin system services restic maintenance)
    (johnlepikhin system services monitoring textfile)))

(define (maintenance-program config)
  (match-record config <restic-maintenance-configuration>
    (restic user data-directory cache-directory jobs textfile-directory)
    (program-file
     "restic-maintenance"
     (with-extensions (list guile-json-4)
       (with-imported-modules %maintenance-modules
         #~(begin
             (use-modules (johnlepikhin system services restic maintenance))
             (run-restic-maintenance #$textfile-directory
                                     #$(file-append restic "/bin/restic")
                                     #$(file-append coreutils "/bin/timeout")
                                     #$user #$data-directory
                                     #$cache-directory
                                     '#$(map job->alist jobs))))))))

(define (freshness-program config)
  (match-record config <restic-maintenance-configuration>
    (data-directory jobs textfile-directory)
    (program-file
     "restic-freshness"
     (with-extensions (list guile-json-4)
       (with-imported-modules %maintenance-modules
         #~(begin
             (use-modules (johnlepikhin system services restic maintenance))
             (collect-restic-freshness
              #$textfile-directory #$data-directory
              '#$(map restic-maintenance-job-repository jobs))))))))

(define (restic-maintenance-mcron-jobs config)
  (list #~(job #$(restic-maintenance-configuration-schedule config)
               #$(maintenance-program config))
        #~(job "*/5 * * * *" #$(freshness-program config))))

(define (restic-maintenance-activation config)
  (match-record config <restic-maintenance-configuration>
    (user group cache-directory jobs)
    #~(begin
        #$(data-directory-activation cache-directory user group)
        #$@(map (lambda (job)
                  (secret-file-check (restic-maintenance-job-key-file job)
                                     user))
                jobs))))

(define restic-maintenance-service-type
  (service-type
   (name 'restic-maintenance)
   (extensions
    (list (service-extension mcron-service-type
                             restic-maintenance-mcron-jobs)
          (service-extension activation-service-type
                             restic-maintenance-activation)))
   (description "Forget, prune and check restic repositories on this host
every day with a maintenance key of each, and report their state, and the
freshness of their newest snapshot every five minutes, to the textfile
directory of the node exporter.")))
