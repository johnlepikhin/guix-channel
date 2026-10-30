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

;;; System services of a headless build server: SSH, a tuned guix-daemon,
;;; Docker and periodic maintenance.  Roles such as guix publish, guix
;;; offload or GitLab Runner are separate services the host adds on top.

(define-module (johnlepikhin system server-services)
  #:use-module (gnu services)
  #:use-module (gnu services avahi)
  #:use-module (gnu packages containers)            ; docker-cli
  #:use-module (gnu services base)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)              ; elogind
  #:use-module (gnu services docker)
  #:use-module (gnu services linux)                ; earlyoom
  #:use-module (gnu services mcron)
  #:use-module (gnu services networking)           ; ntp
  #:use-module (gnu services ssh)
  #:use-module (guix gexp)
  #:use-module (json)
  #:use-module (johnlepikhin system services common)
  #:use-module (johnlepikhin system services restart-on-upgrade)
  #:export (%server-substitute-urls
            make-server-services))

;; cache-cdn.guix.moe is behind Cloudflare and serves both Guix and Nonguix
;; substitutes, signed with the Nonguix key.
(define %server-substitute-urls
  '("https://cache-cdn.guix.moe"
    "https://bordeaux.guix.gnu.org"
    "https://ci.guix.gnu.org"))

(define %server-gc-job
  ;; CI and offload builds fill the store fast; keep plenty of headroom and
  ;; drop old system generations that `guix deploy' leaves behind.
  (make-guix-gc-job #:schedule "15 */4 * * *"
                    #:free-space "300G"
                    #:delete-generations "1m"))

;; Nothing else ever removes images pulled for CI jobs, their stopped
;; containers, the build cache or the runner cache volumes.
(define %docker-prune-job
  #~(job "45 3 * * *"
         (let ((docker #$(file-append docker-cli "/bin/docker")))
           (string-append docker " system prune --all --force"
                          " --filter until=168h && "
                          ;; Docker 20.10 prunes named volumes too; from
                          ;; 23 on this needs --all.
                          docker " volume prune --force"))))

(define* (make-server-services
          #:key
          (ssh-authorized-keys '())
          (authorized-keys (list nonguix-signing-key bordeaux-signing-key))
          (substitute-urls %server-substitute-urls)
          (channels #f)
          (guix-extra-options '())
          (docker-daemon-settings '()))
  "Return the base services of a build server.  SSH-AUTHORIZED-KEYS is an
alist of user names to lists of public key files, as in
@code{openssh-configuration}; logins are by key only.  AUTHORIZED-KEYS and
SUBSTITUTE-URLS configure substitutes.  CHANNELS, when true, is written to
/etc/guix/channels.scm.  GUIX-EXTRA-OPTIONS are passed to guix-daemon, e.g.
@code{\"--max-jobs=4\"}.  DOCKER-DAEMON-SETTINGS, an alist, goes to dockerd's
daemon.json, e.g. @code{'((\"live-restore\" . #t))}; dockerd reads it only
when it starts."
  (cons*
   (service openssh-service-type
            (openssh-configuration
             (permit-root-login 'prohibit-password)
             (password-authentication? #f)
             (authorized-keys ssh-authorized-keys)))

   ;; dockerd requires both.
   (service dbus-root-service-type)
   (service elogind-service-type)
   (service containerd-service-type)
   (service docker-service-type
            (if (null? docker-daemon-settings)
                (docker-configuration)
                (docker-configuration
                 (config-file
                  (plain-file "daemon.json"
                              (scm->json-string docker-daemon-settings))))))

   ;; Announces guix publish on the local network.
   (service avahi-service-type)
   (service ntp-service-type)
   (service earlyoom-service-type)

   (service mcron-service-type
            (mcron-configuration
             (jobs (list %server-gc-job fstrim-job %docker-prune-job))))
   ;; Otherwise jobs added by a deploy run only after mcron is restarted.
   (service restart-on-upgrade-service-type)

   prefer-ipv4-service

   (modify-services %base-services
     (guix-service-type
      config => (guix-configuration
                 (inherit config)
                 (substitute-urls substitute-urls)
                 (authorized-keys (append authorized-keys
                                          %default-authorized-guix-keys))
                 (channels (or channels (guix-configuration-channels config)))
                 ;; Keep build dependencies of live store items, so that
                 ;; rebuilds after a GC and guix publish do not refetch
                 ;; them.
                 (extra-options
                  (append (list "--gc-keep-derivations=yes"
                                "--gc-keep-outputs=yes")
                          guix-extra-options)))))))
