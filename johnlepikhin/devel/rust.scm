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

(define-module (johnlepikhin devel rust)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (johnlepikhin packages rust-nightly)
  #:use-module (srfi srfi-1)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:export (home-devel-rust-configuration
            home-devel-rust-service-type))

(define-record-type* <home-devel-rust-configuration>
  home-devel-rust-configuration make-home-devel-rust-configuration
  home-devel-rust-configuration?
  (package home-devel-rust-configuration-package (default rust-nightly-2022.08.31))
  (package-clippy home-devel-rust-configuration-package-clippy (default clippy-nightly-2022.08.31))
  (package-src home-devel-rust-configuration-package-src (default rust-src-nightly-2022.08.31)))

(define (add-devel-rust-packages config)
  (list
   (home-devel-rust-configuration-package config)
   (home-devel-rust-configuration-package-clippy config)
   (home-devel-rust-configuration-package-src config)))

(define (add-env-variables config)
  `(("RUST_SRC_PATH" . ,(file-append (getenv "HOME") "/.guix-home/profile/lib/rustlib/src/rust/library"))
    ("LIBCLANG_PATH" . ,(file-append (getenv "HOME") "/.guix-home/profile/lib"))))

(define home-devel-rust-service-type
  (service-type
   (name 'home-devel-rust)
   (extensions
    (list
     (service-extension home-profile-service-type add-devel-rust-packages)
     (service-extension home-environment-variables-service-type add-env-variables)))
   (compose concatenate)
   (description "Install packages required for Rust development")))
