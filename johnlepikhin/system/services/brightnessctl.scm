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

(define-module (johnlepikhin system services brightnessctl)
  #:use-module (gnu packages linux)
  #:use-module (guix gexp)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services))

;;; Constants

(define %backlight-brightness-path
  "/sys/class/backlight/*/brightness")

(define %system-chmod-path
  "/run/current-system/profile/bin/chmod")

(define %brightness-permissions
  "666")

;;; Service implementation

(define-public (public-backlight-brightness-service _)
  "Create a one-shot shepherd service that grants read/write access to
backlight brightness controls for all users.

This is useful for allowing non-root users to adjust screen brightness
without requiring sudo or setuid binaries."
  (list (shepherd-service
         (provision '(public-backlight-brightness))
         (documentation "Grant R/W to backlight brightness controls for all users")
         (one-shot? #t)
         (start #~(lambda _
                    (system (string-append #$%system-chmod-path
                                          " "
                                          #$%brightness-permissions
                                          " "
                                          #$%backlight-brightness-path)))))))

(define (add-brightnessctl-package config)
  (list brightnessctl))

(define-public public-backlight-brightness-service-type
  (service-type
   (name 'public-backlight-brightness)
   (description "Grant R/W to /sys/class/backlight/*/brightness for all users")
   (extensions
    (list
     (service-extension
      profile-service-type add-brightnessctl-package)
     (service-extension
      shepherd-root-service-type public-backlight-brightness-service)))))
