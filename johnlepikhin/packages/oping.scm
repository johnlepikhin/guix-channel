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

(define-module (johnlepikhin packages oping)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu))

(define (liboping version commit checksum)
  (package
   (name "liboping")
   (version version)
   (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/johnlepikhin/liboping")
                    (commit commit)))
             (sha256 (base32 checksum))))
   (build-system gnu-build-system)
   (supported-systems '("x86_64-linux"))
   (native-inputs `(("perl" ,perl)
                    ("autoconf" ,autoconf)
                    ("libtool" ,libtool)
                    ("pkg-config" ,pkg-config)
                    ("automake" ,automake)))
   (synopsis "octo's ping library")
   (description "octo's ping library")
   (home-page "https://noping.cc")
   (license lgpl2.1)))

(define-public liboping-1.10.1
  (liboping "1.10.1" "7a6e4795c5fb1a98624c3a1c922da87b44476200" "1nmhi4652izd0adrs72m038cy035snn7c8kmhrafx2zw7vwwnrgj"))

(define-public liboping liboping-1.10.1)
