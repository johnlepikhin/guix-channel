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

;;; Restart chosen shepherd services after a reconfigure or a deploy.
;;;
;;; Shepherd only loads the new definitions of running services as pending
;;; replacements: a running service keeps its old configuration until it is
;;; restarted.  For a daemon that is noticed; for mcron or a timer it is
;;; not, and new or changed jobs silently never run.
;;;
;;; The activation spawns a transient service that waits for the new
;;; definitions to be loaded (activation runs before that), then restarts
;;; the listed services, waiting while a timer is executing its action.
;;; Only services that are cheap and safe to restart belong here: a
;;; restart of mcron interrupts the jobs it is running.

(define-module (johnlepikhin system services restart-on-upgrade)
  #:use-module (gnu packages admin)                ; shepherd-1.0
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:export (restart-on-upgrade-program
            restart-on-upgrade-activation
            restart-on-upgrade-service-type))

(define (restart-on-upgrade-program herd services)
  "A program that restarts SERVICES, symbols, with HERD once the shepherd
listening on the socket given as its argument has their new definitions."
  (program-file
   "restart-on-upgrade"
   #~(begin
      (use-modules (ice-9 popen)
                   (ice-9 textual-ports)
                   (srfi srfi-1))

      ;; Shepherd timestamps the lines of the log.
      (define (say fmt . args)
        (apply format #t (string-append fmt "~%") args)
        (force-output))

      (define (herd . args)
        (cons* #$herd (string-append "--socket=" socket) args))

      (define (status service)
        (let* ((port (apply open-pipe* OPEN_READ
                            (herd "status" (symbol->string service))))
               (text (get-string-all port)))
          (and (zero? (status:exit-val (close-pipe port)))
               text)))

      (define (pending? service)
        (let ((text (status service)))
          (and text (string-contains text "Replacement pending"))))

      (define (busy? service)
        (let ((text (status service)))
          (and text (string-contains text "Executing timer action"))))

      (define (wait-until done? seconds)
        (let loop ((left seconds))
          (cond ((done?) #t)
                ((<= left 0) #f)
                (else (sleep 10) (loop (- left 10))))))

      (define socket (cadr (command-line)))

      (setenv "LC_ALL" "C")
      ;; Activation runs before the new definitions are loaded.
      (sleep 30)
      (for-each
       (lambda (service)
         (cond
          ((not (wait-until (lambda () (pending? service)) 300))
           (say "~a: no new definition, left alone" service))
          ((not (wait-until (lambda () (not (busy? service)))
                            (* 6 3600)))
           (say "~a: still busy, restart it by hand" service))
          ((zero? (apply system* (herd "restart"
                                       (symbol->string service))))
           (say "~a: restarted" service))
          (else
           (say "~a: restart failed" service))))
       '#$services))))

(define* (restart-on-upgrade-activation services
                                        #:key herd socket log-file)
  "A gexp for an activation that restarts SERVICES in the background, when
the shepherd listening on SOCKET, a gexp, is running."
  (if (null? services)
      #~#t
      #~(let ((socket #$socket))
          (when (file-exists? socket)
            (system* #$herd (string-append "--socket=" socket)
                     "--service-name=restart-on-upgrade"
                     (string-append "--log-file=" #$log-file)
                     "spawn" "transient" "--"
                     #$(restart-on-upgrade-program herd services)
                     socket)))))

(define (system-activation services)
  (restart-on-upgrade-activation
   (delete-duplicates services)
   #:herd (file-append shepherd-1.0 "/bin/herd")
   #:socket "/var/run/shepherd/socket"
   #:log-file "/var/log/restart-on-upgrade.log"))

(define restart-on-upgrade-service-type
  (service-type
   (name 'restart-on-upgrade)
   (extensions
    (list (service-extension activation-service-type system-activation)))
   (compose concatenate)
   (extend append)
   (default-value '(mcron))
   (description "Restart the given shepherd services, such as mcron, after a
reconfigure, so that they run with their new definitions.")))
