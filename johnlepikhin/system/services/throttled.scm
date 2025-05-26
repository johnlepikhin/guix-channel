;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Evgenii Lepikhin <johnlepikhin@gmail.com>
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

(define-module (johnlepikhin system services throttled)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (johnlepikhin packages throttled)
  #:export (throttled-service-type
            throttled-configuration))

(define-record-type* <throttled-configuration>
  throttled-configuration
  make-throttled-configuration
  throttled-configuration?
  (package              throttled-configuration-package
                        (default throttled))
  (config-file          throttled-configuration-config-file
                        (default #f)))

(define (throttled-activation config)
  (let ((package (throttled-configuration-package config))
        (config-file (throttled-configuration-config-file config)))
    (with-imported-modules '((guix build utils))
      (let ((etc-dir "/etc/throttled")
            (throttled.conf "/etc/throttled/throttled.conf"))
        #~(begin
            (use-modules (guix build utils))
            (mkdir-p #$etc-dir)
            (copy-file #$config-file #$throttled.conf))))))

(define-public (throttled-shepherd-service config)
  (let ((package (throttled-configuration-package config))
        (config-file (throttled-configuration-config-file config)))
    (list (shepherd-service
           (provision '(throttled))
           (documentation "Throttled service")
           (start #~(make-forkexec-constructor
                     (list #$(file-append package "/bin/throttled")
                           "--config" #$config-file
                           "--monitor" "10")))
           (stop #~(make-kill-destructor))))))

(define-public throttled-service-type
  (service-type
   (name 'throttled)
   (description "Throttled service")
   (extensions
    (list
     (service-extension activation-service-type
                        throttled-activation)
     (service-extension profile-service-type
                        (compose list throttled-configuration-package))
     (service-extension shepherd-root-service-type
                        throttled-shepherd-service)))))
