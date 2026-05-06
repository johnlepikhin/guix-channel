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

(define-module (johnlepikhin home rutracker-cli)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (johnlepikhin packages rutracker-cli)
  #:use-module (guix records)
  #:export (home-rutracker-cli-configuration
            home-rutracker-cli-service-type))

(define-record-type* <home-rutracker-cli-configuration>
  home-rutracker-cli-configuration make-home-rutracker-cli-configuration
  home-rutracker-cli-configuration?
  (package home-rutracker-cli-configuration-package (default rutracker-cli)))

(define (add-rutracker-cli-package config)
  (list (home-rutracker-cli-configuration-package config)))

(define home-rutracker-cli-service-type
  (service-type
   (name 'home-rutracker-cli)
   (extensions
    (list
     (service-extension home-profile-service-type add-rutracker-cli-package)))
   (default-value (home-rutracker-cli-configuration))
   (description "Install rutracker-cli into the home profile.")))
