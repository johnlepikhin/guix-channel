;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Evgenii Lepikhin <johnlepikhin@gmail.com>
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
  #:use-module (gnu services dbus)
  #:use-module (johnlepikhin packages throttled)
  #:use-module (johnlepikhin system services utils)
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
  "Generate activation script to install throttled configuration file.
This ensures the configuration file is copied to /etc/throttled during system activation."
  (make-config-file-activation
   (throttled-configuration-config-file config)
   "/etc/throttled"
   "throttled.conf"))

(define-public (throttled-shepherd-service config)
  "Create shepherd service for throttled.
Throttled is a workaround for Intel CPU throttling issues that overrides
power limits and temperature trip points to prevent excessive throttling."
  (let ((package (throttled-configuration-package config))
        (config-file (throttled-configuration-config-file config)))
    (list (shepherd-service
           (provision '(throttled))
           (requirement '(dbus-system udev))
           (documentation "Fix for Intel CPU throttling on Linux")
           (start #~(make-forkexec-constructor
                     (list #$(file-append package "/bin/throttled")
                           "--config" #$config-file)))
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
