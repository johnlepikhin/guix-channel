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

(define-module (johnlepikhin home urxvt)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu home services)
  #:use-module (johnlepikhin home xsession)
  #:use-module (johnlepikhin home xresources)
  #:use-module (srfi srfi-1)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:export (home-urxvt-configuration
            home-urxvt-service-type))

(define-record-type* <home-urxvt-configuration>
  home-urxvt-configuration make-home-urtxvt-configuration
  home-urtxvt-configuration?
  (package home-urtxvt-configuration-package (default rxvt-unicode)))

(define (add-urxvt-package config)
  (list (home-urtxvt-configuration-package config)))

(define (add-xsession-component config)
  "urxvtd &")

(define (add-extensions config)
  `((".urxvt/ext/set-font-size" ,(local-file "files/urxvt/set-font-size.pl"))
    (".urxvt/ext/edit-screen" ,(local-file "files/urxvt/edit-screen.pl"))))

(define (add-xresources config)
  (list
   (home-xresources-record (name "URxvt*saveLines") (value "5000"))
   (home-xresources-record (name "URxvt*font") (value "xft:DejaVu Sans Mono-12"))
   (home-xresources-record (name "URxvt*background") (value "black"))
   (home-xresources-record (name "URxvt*foreground") (value "gray"))
   (home-xresources-record (name "URxvt*scrollstyle") (value "rxvt"))
   (home-xresources-record (name "URxvt*termName") (value "xterm"))
   (home-xresources-record (name "URxvt*perl-ext-common") (value "set-font-size,matcher,edit-screen,selection"))
   (home-xresources-record (name "URxvt.url-launcher") (value "xdg-open"))
   (home-xresources-record (name "URxvt.matcher.button") (value "3"))
   (home-xresources-record (name "URxvt.colorUL") (value "#86a2be"))
   (home-xresources-record (name "URxvt*secondaryScroll") (value "true"))
   (home-xresources-record (name "URxvt*fading") (value "40"))
   (home-xresources-record (name "URxvt*scrollBar") (value "false"))
   (home-xresources-record (name "URxvt.keysym.C-Up") (value "perl:font-size:increase"))
   (home-xresources-record (name "URxvt.keysym.C-Down") (value "perl:font-size:decrease"))
   (home-xresources-record (name "URxvt*iso14755") (value "False"))
   (home-xresources-record (name "URxvt*iso14755_52") (value "False"))
   (home-xresources-record (name "URxvt*keysym.0xff50") (value "\\033OH"))
   (home-xresources-record (name "URxvt*keysym.0xff57") (value "\\033OF"))
   (home-xresources-record (name "!URxvt*keysym.0xff55") (value "\\033[5~"))
   (home-xresources-record (name "!URxvt*keysym.0xff56") (value "\\033[6~"))
   (home-xresources-record (name "*Rxvt*meta8") (value "false"))
   (home-xresources-record (name "*Rxvt.keysym.C-Left") (value "\\033[1;5D"))
   (home-xresources-record (name "*Rxvt.keysym.C-Right") (value "\\033[1;5C"))
   (home-xresources-record (name "*Rxvt.keysym.M-Left") (value "\\033[1;3D"))
   (home-xresources-record (name "*Rxvt.keysym.M-Right") (value "\\033[1;3C"))
   (home-xresources-record (name "URxvt*print-pipe") (value "cat >/dev/null"))))

(define home-urxvt-service-type
  (service-type
   (name 'home-urxvt)
   (extensions
    (list
     (service-extension
      home-files-service-type add-extensions)
     (service-extension
      home-profile-service-type add-urxvt-package)
     (service-extension
      home-xresources-service-type add-xresources)
     (service-extension
      home-xsession-service-type
      add-xsession-component)))
   (compose concatenate)
   (description "Install urxvt and add configs")))
