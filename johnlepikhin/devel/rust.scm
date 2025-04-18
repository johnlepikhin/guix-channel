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
  #:use-module (gnu home services)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages tls)
  #:use-module (gnu services configuration)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:export (home-devel-rust-configuration
            home-devel-rust-service-type))

(define-record-type* <home-devel-rust-configuration>
  home-devel-rust-configuration make-home-devel-rust-configuration
  home-devel-rust-configuration?
  (package home-devel-rust-configuration-package (default rust))
  (edition home-devel-rust-configuration-edition (default "2021")))

(define (add-devel-rust-packages config)
  (let ((rust-package (home-devel-rust-configuration-package config)))
    (list
     clang
     openssl-1.1
     rust-package
     (list rust-package "tools")
     (list rust-package "rust-src")
     (list rust-package "cargo"))))

(define (add-env-variables config)
  `(("RUST_SRC_PATH" . ,(string-append (getenv "HOME") "/.guix-home/profile/lib/rustlib/src/rust/library"))
    ("LIBCLANG_PATH" . ,(string-append (getenv "HOME") "/.guix-home/profile/lib"))))

(define (add-files config)
  `((".rustfmt.toml"
     ,(plain-file "rustfmt.toml"
                  (format #f "edition = \"~a\"\n"
                          (home-devel-rust-configuration-edition config))))))

(define home-devel-rust-service-type
  (service-type
   (name 'home-devel-rust)
   (extensions
    (list
     (service-extension home-profile-service-type add-devel-rust-packages)
     (service-extension home-environment-variables-service-type add-env-variables)
     (service-extension home-files-service-type add-files)))
   (compose concatenate)
   (description "Install packages required for Rust development")))
