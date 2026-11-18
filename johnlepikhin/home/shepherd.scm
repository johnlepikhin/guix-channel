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

;;; Module: (johnlepikhin home shepherd)
;;;
;;; The user's Shepherd, started on first login without the login's output.
;;;
;;; Upstream's on-first-login starts shepherd with `system*', so the daemon
;;; keeps the stdout and stderr of the shell that sourced ~/.profile.  For
;;; `ssh host command' those are the pipes of the ssh session -- Guix's
;;; ~/.bashrc sources ~/.profile for non-interactive ssh shells -- and sshd
;;; closes a session only at their EOF.  The first command of each new login
;;; therefore never returns.  A login is new whenever the user had no
;;; session left: elogind then removes /run/user/UID with the flag file of
;;; on-first-login.
;;;
;;; Here shepherd starts with stdin, stdout and stderr on /dev/null; it logs
;;; to its own log file anyway.  Upstream's start on activation is dropped
;;; with auto-start?: it only matters when shepherd died, and the next
;;; first login starts it again.

(define-module (johnlepikhin home shepherd)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (home-detached-shepherd-services))

(define (launch-shepherd-gexp shepherd)
  #~(let ((runtime-directory (or (getenv "XDG_RUNTIME_DIR")
                                 (format #f "/run/user/~a" (getuid))))
          (configuration-directory (or (getenv "XDG_CONFIG_HOME")
                                       (string-append (getenv "HOME")
                                                      "/.config"))))
      (unless (file-exists? (string-append runtime-directory
                                           "/shepherd/socket"))
        (let ((null (open-file "/dev/null" "r+")))
          ;; Shepherd daemonizes once its configuration is loaded, so this
          ;; waits for the start, as upstream's `system*' does.
          (waitpid
           (spawn #$(file-append shepherd "/bin/shepherd")
                  (list "shepherd" "--config"
                        (string-append configuration-directory
                                       "/shepherd/init.scm"))
                  #:input null #:output null #:error null))
          (close-port null)))))

(define home-shepherd-launch-service-type
  (service-type
   (name 'home-shepherd-launch)
   (extensions
    (list (service-extension home-run-on-first-login-service-type
                             launch-shepherd-gexp)))
   (description "Start the user's Shepherd on first login, detached from
the output of the login.")))

(define* (home-detached-shepherd-services
          #:optional (config (home-shepherd-configuration)))
  "Return the services of the user's Shepherd configured by CONFIG, a
<home-shepherd-configuration>, that start it on first login detached from the
login's output, unless CONFIG disables auto-start."
  (if (home-shepherd-configuration-auto-start? config)
      (list (service home-shepherd-service-type
                     (home-shepherd-configuration
                      (inherit config)
                      (auto-start? #f)))
            (service home-shepherd-launch-service-type
                     (home-shepherd-configuration-shepherd config)))
      (list (service home-shepherd-service-type config))))
