;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Evgenii Lepikhin <johnlepikhin@gmail.com>
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

(define-module (johnlepikhin home xresources)
  #:use-module (gnu home services)
  #:use-module (srfi srfi-1)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (gnu home services shells)
  #:use-module (johnlepikhin home xsession)
  #:export (home-xresources-service-type
            home-xresources-configuration
            home-xresources-record))

(define-record-type* <home-xresources-record>
  home-xresources-record make-xresources-record
  home-xresources-record?
  (name home-xresources-record-name)
  (value home-xresources-record-value)
  (comment home-xresources-record-comment (default #f)))

(define-record-type* <home-xresources-configuration>
  home-xresources-configuration make-xresources-configuration
  home-xresources-configuration?
  (records home-xresources-configuration-records (default '())))

(define (serialize-home-xresources-record record)
  (string-append
   (if (home-xresources-record-comment record)
       (format #f "! ~a\n" (home-xresources-record-comment record)) "")
   (format #f "~a: ~a\n"
           (home-xresources-record-name record)
           (home-xresources-record-value record))))

(define (add-xresources-file config)
  `((".config/xresources"
     ,(mixed-text-file
       "config-xresources"
       #~(string-append
          #$@(map
              serialize-home-xresources-record
              (home-xresources-configuration-records config)))))))

(define (add-xresources-extensions config extensions)
  (home-xresources-configuration
   (inherit config)
   (records
    (append (home-xresources-configuration-records config)
            extensions))))

(define (merge-command _)
  (string-append "xrdb -merge <" (getenv "HOME") "/.config/xresources"))

(define (home-xresources-activation config)
  `(("files/.config/xresources" ,#~(system #$(merge-command #t)))))

(define (add-xsession-component config)
  (xsession-component
   (command (merge-command #t))
   (priority 10)))

(define home-xresources-service-type
  (service-type
   (name 'home-xresources)
   (extensions
    (list
     (service-extension home-files-service-type add-xresources-file)
     (service-extension home-run-on-change-service-type home-xresources-activation)
     (service-extension home-xsession-service-type add-xsession-component)))
   (compose concatenate)
   (extend add-xresources-extensions)
   (description "Creates @file{~/.config/xresources} and configures autoloading for it")))
