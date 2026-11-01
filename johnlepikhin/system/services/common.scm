;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023-2026 Evgenii Lepikhin <johnlepikhin@gmail.com>
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

;;; Building blocks shared by the desktop and the server system services.

(define-module (johnlepikhin system services common)
  #:use-module (gnu packages containers)           ; runc
  #:use-module (gnu packages guile)                ; guile-json-4
  #:use-module (gnu packages linux)                ; util-linux
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (nonguix-signing-key
            bordeaux-signing-key
            make-guix-gc-job
            make-system-generations-job
            guix-gc-job
            fstrim-job
            prefer-ipv4-service
            docker-daemon-config-file
            docker-runtime-migration-service))

;; Also signs the substitutes served by cache-cdn.guix.moe.
(define nonguix-signing-key
  (plain-file "non-guix.pub"
   "(public-key
     (ecc
       (curve Ed25519)
       (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)
     ))"))

(define bordeaux-signing-key
  (plain-file "bordeaux.guix.gnu.org.pub"
   "(public-key
     (ecc
       (curve Ed25519)
       (q #7D602902D3A2DBB83F8A0FB98602A754C5493B0B778C8D1DD4E0F41DE14DE34F#)
     ))"))

(define* (make-guix-gc-job #:key
                           (schedule "30 4   * * *")
                           (free-space "30G")
                           (delete-generations #f))
  "Return an mcron job that collects garbage until FREE-SPACE is available
and, when DELETE-GENERATIONS is a pattern such as \"1m\", first deletes
profile generations matching it."
  #~(job #$schedule
         #$(string-append "guix gc -F " free-space
                          (if delete-generations
                              (string-append " -d " delete-generations)
                              ""))))

(define guix-gc-job (make-guix-gc-job))

(define* (make-system-generations-job #:key
                                      (schedule "20 12,18 * * *")
                                      (pattern "1m"))
  "Return an mcron job deleting the system generations matching PATTERN,
such as \"1m\", with `guix system delete-generations', which also drops
them from the boot menu; `guix gc -d' alone would leave entries pointing
at deleted generations."
  #~(job #$schedule
         #$(string-append "guix system delete-generations " pattern)))

;; trim free blocks on SSD.  mcron's PATH has no sbin directory.
(define fstrim-job
  #~(job "40 */2   * * *"
         (string-append #$(file-append util-linux "/sbin/fstrim") " -v /")))

;; IPv6 to Cloudflare (gitlab.com and others) times out on some networks,
;; while libgit2 tries addresses in getaddrinfo order without Happy Eyeballs,
;; so `guix pull' fails on nonguix.  Prefer IPv4.
(define prefer-ipv4-service
  (simple-service 'prefer-ipv4 etc-service-type
                  (list `("gai.conf"
                          ,(plain-file "gai.conf"
                                       "precedence ::ffff:0:0/96 100\n")))))

;; Guix patches dockerd so that the name of its stock runtime is the store
;; file name of runc.  dockerd records that name in the hostconfig.json of
;; every container it creates, so once runc is upgraded that name matches no
;; runtime and these containers refuse to start with "Unknown runtime
;; specified".  A runtime declared in daemon.json under a fixed name is
;; recorded by that name instead, and its path is looked up each time dockerd
;; starts.  The name "runc" is free: the reserved one is the store file name.
(define* (docker-daemon-config-file #:optional (settings '()))
  "Return a daemon.json for dockerd holding the alist SETTINGS together with
a default runtime whose name survives upgrades of runc."
  (for-each (lambda (key)
              (when (assoc key settings)
                (error "docker-daemon-config-file: setting is managed here:"
                       key)))
            '("runtimes" "default-runtime"))
  (computed-file
   "daemon.json"
   (with-extensions (list guile-json-4)
     #~(begin
         (use-modules (json))
         (call-with-output-file #$output
           (lambda (port)
             (scm->json
              `(("runtimes"
                 ("runc" ("path" . #$(file-append runc "/sbin/runc"))))
                ("default-runtime" . "runc")
                ,@'#$settings)
              port)))))))

;; Containers created before `docker-daemon-config-file' was in use, or by a
;; dockerd started without it, record the runtime by its store file name.
;; Rename it to "runc" at boot, before dockerd starts: a running dockerd
;; writes its own idea of hostconfig.json back when it stops, so an edit made
;; under it is lost.  On a reconfigure dockerd keeps running, and the rename
;; waits for the next boot; until then the old name still matches the stock
;; runtime as long as runc is not upgraded and dockerd not restarted.
(define docker-runtime-migration-service
  (simple-service
   'docker-runtime-migration activation-service-type
   #~(begin
       (use-modules (ice-9 ftw)
                    (ice-9 regex)
                    (ice-9 textual-ports)
                    (srfi srfi-1))

       (define (dockerd-running?)
         (any (lambda (pid)
                (let ((comm (string-append "/proc/" pid "/comm")))
                  (false-if-exception
                   (string=? "dockerd"
                             (call-with-input-file comm
                               (compose string-trim-both get-string-all))))))
              (or (scandir "/proc" (lambda (name)
                                     (string-every char-set:digit name)))
                  '())))

       (define store-runtime
         (make-regexp "\"Runtime\":\"/gnu/store/[^\"]*/runc\""))

       (define (migrate file)
         (let* ((text (call-with-input-file file get-string-all))
                (new (regexp-substitute/global #f store-runtime text
                                               'pre "\"Runtime\":\"runc\""
                                               'post)))
           (unless (string=? text new)
             (let ((tmp (string-append file ".tmp")))
               (call-with-output-file tmp
                 (lambda (port) (put-string port new)))
               (chmod tmp (stat:perms (stat file)))
               (rename-file tmp file)
               (format #t "docker: runtime of ~a renamed to runc~%"
                       (basename (dirname file)))))))

       (let ((dir "/var/lib/docker/containers"))
         (when (and (file-exists? dir) (not (dockerd-running?)))
           (for-each (lambda (id)
                       (let ((file (string-append dir "/" id
                                                  "/hostconfig.json")))
                         (when (file-exists? file)
                           (migrate file))))
                     (or (scandir dir (lambda (name)
                                        (not (member name '("." "..")))))
                         '())))))))
