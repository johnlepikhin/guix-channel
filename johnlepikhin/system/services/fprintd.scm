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

(define-module (johnlepikhin system services fprintd)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services dbus)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system pam)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages freedesktop)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module (guix modules)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (fprintd-configuration
            fprintd-configuration?
            fprintd-service-type))

(define-configuration/no-serialization fprintd-configuration
  (fprintd      (file-like fprintd)
                "The fprintd package")
  (unlock-gdm?
   (boolean #t)
   "Generate PAM configuration that unlocks gdm with fprintd.")
  (unlock-other
   (list '("polkit-1" "sddm")) ;; polkit-1 is the name of a PAM module for GNOME polkit
   "List of other PAM modules that can be unlocked with fprintd.

This depends on your desktop configuration. If you for example want GNOME
rompts to be unlocked by fingerprint, you add @code{polkit-1} to this list.
(This is enabled by default.)"))

(define (fprintd-pam-other-services config fprintd-module)
  (pam-extension
   (transformer
    (lambda (pam)
      (if (member (pam-service-name pam)
                  (fprintd-configuration-unlock-other config))
          (let ((sufficient
                 (pam-entry
                  (control "sufficient")
                  (module fprintd-module))))
            (pam-service
             (inherit pam)
             (auth (cons sufficient (pam-service-auth pam)))))
          pam)))))

(define (fprintd-pam-gdm-services fprintd-module)
  (list
   (pam-service
    (inherit (unix-pam-service "gdm-fingerprint"
                               #:login-uid? #t))
    (auth (list
           (pam-entry
            (control "required")
            (module fprintd-module)))))))

(define (fprintd-pam-services config)
  (let ((fprintd-module
         (file-append (fprintd-configuration-fprintd config) "/lib/security/pam_fprintd.so")))
    (cons
     (fprintd-pam-other-services config fprintd-module)
     (if (fprintd-configuration-unlock-gdm? config)
         (fprintd-pam-gdm-services fprintd-module)
         '()))))

(define (fprintd-dbus-service config)
  (list (fprintd-configuration-fprintd config)))

(define fprintd-service-type
  (service-type (name 'fprintd)
                (extensions
                 (list (service-extension dbus-root-service-type
                                          fprintd-dbus-service)
                       (service-extension polkit-service-type
                                          fprintd-dbus-service)
                       (service-extension pam-root-service-type
                                          fprintd-pam-services)))
                (default-value (fprintd-configuration))
                (description
                 "Run fprintd, a fingerprint management daemon.")))
