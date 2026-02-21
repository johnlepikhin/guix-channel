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

(define-module (johnlepikhin home voice-type)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (johnlepikhin packages voice-type)
  #:use-module (guix records)
  #:export (home-voice-type-configuration
            home-voice-type-service-type))

(define-record-type* <home-voice-type-configuration>
  home-voice-type-configuration make-home-voice-type-configuration
  home-voice-type-configuration?
  (package home-voice-type-configuration-package (default voice-type)))

(define (add-voice-type-package config)
  (list (home-voice-type-configuration-package config)))

(define home-voice-type-service-type
  (service-type
   (name 'home-voice-type)
   (extensions
    (list
     (service-extension home-profile-service-type add-voice-type-package)))
   (default-value (home-voice-type-configuration))
   (description "Install voice-type")))
