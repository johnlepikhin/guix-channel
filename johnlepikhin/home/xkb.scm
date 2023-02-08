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

(define-module (johnlepikhin home xkb)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (gnu packages xorg)
  #:use-module (johnlepikhin home xsession)
  #:use-module (srfi srfi-1)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:export (home-xkb-configuration
            home-xkb-service-type))

(define-record-type* <home-xkb-configuration>
  home-xkb-configuration make-home-xkb-configuration
  home-xkb-configuration?)

(define (add-xkb-files config)
  `((".config/xkb/symbols/my_us" ,(local-file "files/xkb/symbols/my_us"))
    (".config/xkb/symbols/my_ru" ,(local-file "files/xkb/symbols/my_ru"))))

(define (add-xkb-package config)
  (list xkbcomp setxkbmap))

(define (activation-command _)
  (string-append "setxkbmap -symbols my_us -print | xkbcomp -I" (getenv "HOME") "/.config/xkb - $DISPLAY"))

(define (home-xkb-activation config)
  `(("files/.config/xresources" ,#~(system #$(activation-command #t)))))

(define (add-xsession-component config)
  (activation-command #t))

(define home-xkb-service-type
  (service-type
   (name 'home-xkb)
   (extensions
    (list
     (service-extension
      home-files-service-type add-xkb-files)
     (service-extension
      home-profile-service-type add-xkb-package)
     (service-extension
      home-run-on-change-service-type home-xkb-activation)
     (service-extension
      home-xsession-service-type add-xsession-component)))
   (compose concatenate)
   (description "Create @file{~/.config/xkb/*}")))
