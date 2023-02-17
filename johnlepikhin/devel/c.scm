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

(define-module (johnlepikhin devel c)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages valgrind)
  #:use-module (srfi srfi-1)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:export (home-devel-c-configuration
            home-devel-c-service-type))

(define-record-type* <home-devel-c-configuration>
  home-devel-c-configuration make-home-devel-c-configuration
  home-devel-c-configuration?)

(define (add-devel-c-packages config)
  (list
   (gcc
    clang
    cmake
    gnu-make
    gdb
    valgrind
    m4
    autoconf
    automake
    libtool)))

(define home-devel-c-service-type
  (service-type
   (name 'home-devel-c)
   (extensions
    (list
     (service-extension
      home-profile-service-type add-devel-c-packages)))
   (compose concatenate)
   (description "Install packages required for C developement")))
