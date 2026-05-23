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

(define-module (johnlepikhin system services accel)
  #:use-module (gnu services base)
  #:export (%accel-udev-rule
            accel-udev-rules-service))

;; The Linux kernel "accel" subsystem exposes DRM compute accelerators
;; (Habana Gaudi, Intel NPU, Google Coral Edge TPU, ...) as
;; /dev/accel/accelN.  By default Guix creates these nodes as root:root
;; with mode 0660, so non-root users cannot open them.
;;
;; This module ships a udev rule that hands /dev/accel/* nodes to a
;; dedicated system group "accel" with mode 0660.  The group is
;; intentionally separate from "render" (which is for GPU compute) so
;; the least-privilege boundary stays sharp.
;;
;; The group itself is created via the #:groups argument of
;; udev-rules-service, which extends account-service-type -- so the
;; consuming operating-system only needs to drop
;; accel-udev-rules-service into its services list and add "accel" to
;; the supplementary-groups of every user that needs access.

(define %accel-udev-rule
  (udev-rule
   "70-accel.rules"
   (string-append
    "KERNEL==\"accel[0-9]*\", SUBSYSTEM==\"accel\", "
    "GROUP=\"accel\", MODE=\"0660\"\n")))

(define accel-udev-rules-service
  (udev-rules-service 'accel %accel-udev-rule #:groups '("accel")))
