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

(define-module (johnlepikhin home synaptics)
  #:use-module (gnu home services)
  #:use-module (gnu packages xorg)
  #:use-module (gnu services configuration)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (johnlepikhin home xsession)
  #:use-module (srfi srfi-1)
  #:export (home-synaptics-configuration
            home-synaptics-record
            home-synaptics-service-type))

(define-record-type* <home-synaptics-record>
  home-synaptics-record make-synaptics-record
  home-synaptics-record?
  (name home-synaptics-record-name)
  (value home-synaptics-record-value))

(define-record-type* <home-synaptics-configuration>
  home-synaptics-configuration make-synaptics-configuration
  home-synaptics-configuration?
  (package home-synaptics-configuration-package (default xf86-input-synaptics))
  (records home-synaptics-configuration-records (default '())))

(define (add-synaptics-extensions config extensions)
  (home-synaptics-configuration
   (inherit config)
   (records
    (append (home-synaptics-configuration-records config)
            extensions))))

(define (serialize-record record)
  (string-append
   " "
   (home-synaptics-record-name record)
   "="
   (home-synaptics-record-value record)))

(define (generate-command config)
  (string-append
   "synclient"
   (apply string-append (map serialize-record (home-synaptics-configuration-records config)))))

(define (add-xsession-component config)
  (generate-command config))

(define (add-synaptics-package config)
  (list (home-synaptics-configuration-package config)))

(define home-synaptics-service-type
  (service-type
   (name 'home-synaptics)
   (extensions
    (list
     (service-extension home-xsession-service-type add-xsession-component)
     (service-extension home-profile-service-type add-synaptics-package)))
   (compose concatenate)
   (extend add-synaptics-extensions)
   (description "Configures synaptics")))
