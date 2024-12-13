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
(define-module (johnlepikhin system services intel-cpu-throttle-fix)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services))

(define-public (intel-cpu-throttle-fix-service _)
  (list (shepherd-service (provision '(intel-cpu-throttle-fix))
                          (requirement '(user-processes))
                          (documentation
                           "On some Intel CPUs, the CPU frequency may be throttled to 400 MHz
after few seconds of high load. There many similar issues on the internet and
this service fixes only one class of them, which is described here:
https://unix.stackexchange.com/a/615343.

Important note: module processor_thermal_device_pci MUST be unloaded after boot.
Blacklisting it in modprobe is not enough.")
                          (one-shot? #t)
                          (start #~(lambda _
                                     (system
                                      "/run/current-system/profile/bin/rmmod processor_thermal_device_pci"))))))

(define-public intel-cpu-throttle-fix-service-type
  (service-type (name 'intel-cpu-throttle-fix)
                (description "Fix Intel CPU throttling issue")
                (extensions (list (service-extension
                                   shepherd-root-service-type
                                   intel-cpu-throttle-fix-service)))))
