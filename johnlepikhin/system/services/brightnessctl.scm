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
(define-module (johnlepikhin system services brightnessctl)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services))

(define-public (public-backlight-brightness-service _)
  (list (shepherd-service
         (provision '(public-backlight-brightness))
         (documentation "Grant R/W to /sys/class/backlight/*/brightness for all users")
         (one-shot? #t)
         (start #~(lambda _ (system "/run/current-system/profile/bin/chmod 666 /sys/class/backlight/*/brightness"))))))

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
