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

(define-module (johnlepikhin home gtk)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (srfi srfi-1)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:export (home-gtk-configuration
            home-gtk-service-type))

(define-record-type* <home-gtk-configuration>
  home-gtk-configuration make-home-gtk-configuration
  home-gtk-configuration?
  (icon-theme-name home-gtk-icon-theme-name (default "Papirus")))

(define (add-gtk-settings-file config)
  `((".config/gtk-3.0/settings.ini"
     ,(mixed-text-file
       "gtk-3.0-settings.ini"
       #~(string-append
          "[Settings]\n"
          "gtk-icon-theme-name = " (home-gtk-icon-theme-name config))))))

(define home-gtk-service-type
  (service-type
   (name 'home-gtk)
   (extensions
    (list
     (service-extension home-files-service-type add-gtk-settings-file)))
   (compose concatenate)
   (description "Create @file{~/.config/gtk-3.0/settings.ini}")))
