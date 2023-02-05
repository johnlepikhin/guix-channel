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

(define-module (johnlepikhin home xcompose)
  #:use-module (gnu home services)
  #:use-module (srfi srfi-1)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:export (home-xcompose-service-type
            home-xcompose-configuration
            home-xcompose-include
            home-xcompose-record))

(define-record-type* <home-xcompose-record>
  home-xcompose-record make-xcompose-record
  home-xcompose-record?
  (value home-xcompose-record-value)
  (comment home-xcompose-record-comment (default #f)))

(define-record-type* <home-xcompose-include>
  home-xcompose-include make-xcompose-include
  home-xcompose-include?
  (value home-xcompose-include-value)
  (comment home-xcompose-include-comment (default #f)))

(define-record-type* <home-xcompose-configuration>
  home-xcompose-configuration make-xcompose-configuration
  home-xcompose-configuration?
  (includes home-xcompose-configuration-includes
            (default (list (home-xcompose-include (value "%S/en_US.UTF-8/Compose"))
                           (home-xcompose-include (value "%L")))))
  (records home-xcompose-configuration-records
           (default (list (home-xcompose-record (value "<Multi_key> <Cyrillic_yu> <Cyrillic_yu>     : \"…\"   ellipsis")
                                                (comment "HORIZONTAL ELLIPSIS"))
                          (home-xcompose-record (value "<Multi_key> <minus> <minus>                 : \"–\"   U2013")
                                                (comment "EN DASH"))
                          (home-xcompose-record (value "<Multi_key> <Cyrillic_BE> <Cyrillic_BE>     : \"«\""))
                          (home-xcompose-record (value "<Multi_key> <Cyrillic_YU> <Cyrillic_YU>     : \"»\""))
                          (home-xcompose-record (value "<Multi_key> <grave>                         : \"́\"")
                                                (comment "ударение (комбинирующееся)"))))))

(define (serialize-home-xcompose-include include)
  (string-append
   "include \""
   (home-xcompose-include-value include)
   "\""
   (if (home-xcompose-include-comment include)
       (format #f " # ~a\n" (home-xcompose-include-comment include)) "\n")))

(define (serialize-home-xcompose-record record)
  (string-append
   (home-xcompose-record-value record)
   (if (home-xcompose-record-comment record)
       (format #f " # ~a\n" (home-xcompose-record-comment record)) "\n")))

(define (add-xcompose-file config)
  `((".XCompose"
     ,(mixed-text-file
       "XCompose"
       #~(string-append
          #$@(map
              serialize-home-xcompose-include
              (home-xcompose-configuration-includes config))
          #$@(map
              serialize-home-xcompose-record
              (home-xcompose-configuration-records config)))))))

(define (add-xcompose-extensions config extensions)
  (home-xcompose-configuration
   (inherit config)
   (includes (append (home-xcompose-configuration-includes config) extensions))
   (records (append (home-xcompose-configuration-records config) extensions))))

(define home-xcompose-service-type
  (service-type
   (name 'home-xcompose)
   (extensions
    (list
     (service-extension home-files-service-type add-xcompose-file)))
   (compose concatenate)
   (extend add-xcompose-extensions)
   (description "Creates @file{~/.XCompose}")))
