;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023, 2026 Evgenii Lepikhin <johnlepikhin@gmail.com>
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
  #:use-module (gnu home services)
  #:use-module (gnu packages xorg)
  #:use-module (johnlepikhin home xsession)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:export (home-xkb-configuration
            home-xkb-service-type
            home-xkb-load-keymap-script-path
            home-xkb-reset-keymap-script-path))

(define xkb-custom-symbols-path ".config/xkb/custom.xkb")
(define custom-symbols-file (local-file "files/xkb/custom.xkb"))
(define xkb-default-symbols-path ".config/xkb/default.xkb")
(define default-symbols-file (local-file "files/xkb/default.xkb"))

(define home-xkb-load-keymap-script-path ".config/xkb/load-keymap.sh")
(define home-xkb-reset-keymap-script-path ".config/xkb/reset-keymap.sh")

(define-record-type* <home-xkb-configuration>
  home-xkb-configuration make-home-xkb-configuration
  home-xkb-configuration?
  (symbols home-xkb-symbols (default custom-symbols-file))
  (repeat-delay home-xkb-repeat-delay (default 300))
  (repeat-rate home-xkb-repeat-rate (default 30)))

(define (make-load-keymap-script config)
  (let ((delay-str (number->string (home-xkb-repeat-delay config)))
        (rate-str (number->string (home-xkb-repeat-rate config))))
    (computed-file
     "load-keymap.sh"
     #~(begin
         (with-output-to-file #$output
           (lambda _
             (display
              (string-append
               "#!/bin/sh\n"
               "xkbcomp \"$HOME/" #$xkb-custom-symbols-path "\" \"$DISPLAY\" >/dev/null 2>&1\n"
               "xset r rate " #$delay-str " " #$rate-str "\n"))))
         (chmod #$output #o755)))))

(define (make-reset-keymap-script config)
  (computed-file
   "reset-keymap.sh"
   #~(begin
       (with-output-to-file #$output
         (lambda _
           (display
            (string-append
             "#!/bin/sh\n"
             "xkbcomp \"$HOME/" #$xkb-default-symbols-path "\" \"$DISPLAY\" >/dev/null 2>&1\n"))))
       (chmod #$output #o755))))

(define (add-xkb-files config)
  `((,xkb-custom-symbols-path ,(home-xkb-symbols config))
    (,xkb-default-symbols-path ,default-symbols-file)
    (,home-xkb-load-keymap-script-path ,(make-load-keymap-script config))
    (,home-xkb-reset-keymap-script-path ,(make-reset-keymap-script config))))

(define (add-xkb-package config)
  (list xkbcomp setxkbmap))

(define (add-xsession-component config)
  (xsession-component
   (command (string-append "$HOME/" home-xkb-load-keymap-script-path))
   (priority 20)))

(define (home-xkb-activation config)
  `((,(string-append "files/" xkb-custom-symbols-path)
     ,(string-append "files/" xkb-default-symbols-path)
     ,#~(system #$(string-append "$HOME/" home-xkb-load-keymap-script-path)))))

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
   (description "Create @file{~/.config/xkb/*}")))
