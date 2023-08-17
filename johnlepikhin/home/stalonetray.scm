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

(define-module (johnlepikhin home stalonetray)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (johnlepikhin home xsession)
  #:use-module (gnu xdisorg stalonetray)
  #:use-module (srfi srfi-1)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:export (home-stalonetray-configuration
            home-stalonetray-service-type))

(define-record-type* <home-stalonetray-configuration>
  home-stalonetray-configuration make-home-stalonetray-configuration
  home-stalonetray-configuration?)

(define (add-stalonetray-settings-file config)
  `((".stalonetrayrc" ,(local-file "files/stalonetrayrc"))))

(define (add-stalonetray-package config)
  (list stalonetray))

(define (add-xsession-component config)
  "stalonetray &")

(define home-stalonetray-service-type
  (service-type
   (name 'home-stalonetray)
   (extensions
    (list
     (service-extension home-files-service-type add-stalonetray-settings-file)
     (service-extension home-profile-service-type add-stalonetray-package)
     (service-extension home-xsession-service-type add-xsession-component)))
   (compose concatenate)
   (description "Install stalonetray and create @file{~/.stalonetrayrc}")))
