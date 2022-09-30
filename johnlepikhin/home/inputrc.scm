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

(define-module (johnlepikhin home inputrc)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (srfi srfi-1)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:export (home-inputrc-record
            home-inputrc-configuration
            home-inputrc-service-type))

(define-record-type* <home-inputrc-record>
  home-inputrc-record make-home-inputrc-record
  home-inputrc-record?
  (key home-inputrc-record-key)
  (function home-inputrc-record-function)
  (comment home-inputrc-record-comment (default #f)))

(define (serialize-home-inputrc-record record)
  (string-append
   (if (home-inputrc-record-comment record)
       (format #f "# ~a\n" (home-inputrc-record-comment record)) "")
   (format #f "~a: ~a\n\n"
           (home-inputrc-record-key record)
           (home-inputrc-record-function record))))

(define-record-type* <home-inputrc-configuration>
  home-inputrc-configuration make-home-inputrc-configuration
  home-inputrc-configuration?
  (records home-inputrc-configuration-records (default '())))

(define (add-inputrc-file config)
  `((".inputrc"
     ,(mixed-text-file
       "inputrc"
       #~(string-append
          #$@(map (lambda (record)
                    (serialize-home-inputrc-record record))
                  (home-inputrc-configuration-records config)))))))

(define (add-inputrc-extensions config extensions)
  (home-inputrc-configuration
   (inherit config)
   (records
    (append (home-inputrc-configuration-records config) extensions))))

(define home-inputrc-service-type
  (service-type
   (name 'home-inputrc)
   (extensions
    (list
     (service-extension home-files-service-type add-inputrc-file)))
   (compose concatenate)
   (extend add-inputrc-extensions)
   (description "Create @file{~/.inputrc}")))
