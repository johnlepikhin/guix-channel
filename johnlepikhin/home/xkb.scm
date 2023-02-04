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
  #:use-module (srfi srfi-1)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:export (home-xkb-configuration
            home-xkb-service-type))

(define-record-type* <home-xkb-configuration>
  home-xkb-configuration make-home-xkb-configuration
  home-xkb-configuration?)

(define (add-xkb-us-file config)
  `((".config/kxb/symbols/my_us"
     ,(mixed-text-file
       "kxb-symbols-my_us"
       "xkb_symbols \"my_us\"  {
        include \"pc+us+inet(evdev)+capslock(menu)+compose(ralt)\"
        key <PRSC> { [ Insert ] };
};"))))

(define (add-xkb-ru-file config)
  `((".config/kxb/symbols/my_ru"
     ,(mixed-text-file
       "kxb-symbols-my_ru"
       "xkb_symbols \"my_ru\"  {
        include \"pc+ru+inet(evdev)+capslock(menu)+compose(ralt)\"
        key <PRSC> { [ Insert ] };
};"))))

(define home-xkb-service-type
  (service-type
   (name 'home-xkb)
   (extensions
    (list
     (service-extension
      home-files-service-type
      (list add-xkb-us-file add-xkb-ru-file))))
   (compose concatenate)
   (description "Create @file{~/.config/kxb/*}")))
