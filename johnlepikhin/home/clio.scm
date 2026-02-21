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

(define-module (johnlepikhin home clio)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (johnlepikhin home xsession)
  #:use-module (johnlepikhin packages clio)
  #:use-module (guix records)
  #:export (home-clio-configuration
            home-clio-service-type))

(define-record-type* <home-clio-configuration>
  home-clio-configuration make-home-clio-configuration
  home-clio-configuration?
  (package home-clio-configuration-package (default clio)))

(define (add-clio-package config)
  (list (home-clio-configuration-package config)))

(define (add-xsession-component config)
  "clio watch &")

(define home-clio-service-type
  (service-type
   (name 'home-clio)
   (extensions
    (list
     (service-extension home-profile-service-type add-clio-package)
     (service-extension home-xsession-service-type add-xsession-component)))
   (default-value (home-clio-configuration))
   (description "Install clio and start clipboard watcher in xsession")))
