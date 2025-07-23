;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Evgenii Lepikhin <johnlepikhin@gmail.com>
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

(define-module (johnlepikhin home numlockx)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu home services)
  #:use-module (johnlepikhin home xsession)
  #:use-module (johnlepikhin home run-on-unlock)
  #:use-module (srfi srfi-1)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:export (home-numlockx-configuration
            home-numlockx-service-type))

(define-record-type* <home-numlockx-configuration>
  home-numlockx-configuration make-home-urtxvt-configuration
  home-urtxvt-configuration?
  (status home-numlockx-configuration-status (default "on")))

(define (add-numlockx-package config)
  (list numlockx))

(define (activation-command config)
  (string-append
   "numlockx "
   (home-numlockx-configuration-status config)))

(define (xsession-component config)
  (xsession-component
   (command (activation-command config))
   (priority 30)))

(define (home-numlockx-activation config)
  ;; TODO monitor dedicated file
  `(("files/.xsession" ,#~(system #$(activation-command config)))))

(define home-numlockx-service-type
  (service-type
   (name 'home-numlockx)
   (extensions
    (list
     (service-extension
      home-profile-service-type add-numlockx-package)
     (service-extension
      home-xsession-service-type xsession-component)
     (service-extension
      home-run-on-unlock-service-type activation-command)
     (service-extension
      home-run-on-change-service-type home-numlockx-activation)))
   (compose concatenate)
   (description "Install numlockx and call it from xsession")))
