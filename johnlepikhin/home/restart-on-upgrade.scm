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

;;; Restart chosen services of the user's shepherd after `guix home
;;; reconfigure', see (johnlepikhin system services restart-on-upgrade).

(define-module (johnlepikhin home restart-on-upgrade)
  #:use-module (gnu home services)
  #:use-module (gnu packages admin)                ; shepherd-for-home
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (johnlepikhin system services restart-on-upgrade)
  #:use-module (srfi srfi-1)
  #:export (home-restart-on-upgrade-service-type))

(define (home-activation services)
  (restart-on-upgrade-activation
   (delete-duplicates services)
   #:herd (file-append shepherd-for-home "/bin/herd")
   #:socket #~(string-append (or (getenv "XDG_RUNTIME_DIR")
                                 (format #f "/run/user/~a" (getuid)))
                             "/shepherd/socket")
   #:log-file #~(string-append (or (getenv "XDG_STATE_HOME")
                                   (string-append (getenv "HOME")
                                                  "/.local/state"))
                               "/log/restart-on-upgrade.log")))

(define home-restart-on-upgrade-service-type
  (service-type
   (name 'home-restart-on-upgrade)
   (extensions
    (list (service-extension home-activation-service-type home-activation)))
   (compose concatenate)
   (extend append)
   (default-value '(mcron))
   (description "Restart the given services of the user's shepherd, such as
mcron, after a reconfigure, so that they run with their new definitions.")))
