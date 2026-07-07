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

(define-module (johnlepikhin system services v4l2loopback)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu services linux)                ; kernel-module-loader-service-type
  #:export (v4l2loopback-configuration
            v4l2loopback-service-type))

;; Load the v4l2loopback kernel module at boot with fixed parameters so a
;; virtual camera device (e.g. /dev/video10) is available for producers such
;; as fluxframe.  The module package itself (`v4l2loopback-linux-module' from
;; (nongnu packages linux)) must be present in the operating-system
;; `kernel-loadable-modules' field — this service only loads it by name and
;; installs its modprobe options; it does not build or ship the module.
;;
;; Mechanism mirrors upstream `zram-device-service-type': extend
;; kernel-module-loader-service-type to modprobe the module at boot, and
;; etc-service-type to drop /etc/modprobe.d/<module>.conf with the options.
;; Guix's modprobe wrapper runs with `-C /etc/modprobe.d', so the options are
;; honoured on load.

(define-record-type* <v4l2loopback-configuration>
  v4l2loopback-configuration
  make-v4l2loopback-configuration
  v4l2loopback-configuration?
  (module-name     v4l2loopback-configuration-module-name
                   (default "v4l2loopback"))
  (devices         v4l2loopback-configuration-devices
                   (default 1))
  (video-nr        v4l2loopback-configuration-video-nr
                   (default 10))
  (card-label      v4l2loopback-configuration-card-label
                   (default "FluxFrame Camera"))
  ;; exclusive_caps=1 makes the device report OUTPUT-only until a producer
  ;; attaches, which is what lets Chrome/Chromium list the loopback camera.
  (exclusive-caps? v4l2loopback-configuration-exclusive-caps?
                   (default #t)))

(define (bool->flag value)
  "Serialise a boolean into the 1/0 form expected by module parameters."
  (if value "1" "0"))

(define (modprobe-options-line config)
  "Render the `options <module> …' line for /etc/modprobe.d.  A quoted
card_label is required because the label contains spaces; modprobe.d honours
double quotes surrounding the whole value."
  (let ((module (v4l2loopback-configuration-module-name config)))
    (string-append
     "options " module
     " devices=" (number->string (v4l2loopback-configuration-devices config))
     " video_nr=" (number->string (v4l2loopback-configuration-video-nr config))
     " card_label=\"" (v4l2loopback-configuration-card-label config) "\""
     " exclusive_caps=" (bool->flag
                         (v4l2loopback-configuration-exclusive-caps? config))
     "\n")))

(define (v4l2loopback-modules config)
  (list (v4l2loopback-configuration-module-name config)))

(define (v4l2loopback-etc config)
  (list (list "modprobe.d/v4l2loopback.conf"
              (plain-file "v4l2loopback.conf"
                          (modprobe-options-line config)))))

(define v4l2loopback-service-type
  (service-type
   (name 'v4l2loopback)
   (description "Load the v4l2loopback kernel module at boot with the
configured parameters and install its modprobe options, providing a virtual
camera device (e.g. @file{/dev/video10}).")
   (extensions
    (list
     (service-extension kernel-module-loader-service-type
                        v4l2loopback-modules)
     (service-extension etc-service-type
                        v4l2loopback-etc)))
   (default-value (v4l2loopback-configuration))))
