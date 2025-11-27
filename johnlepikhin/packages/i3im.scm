;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024-2025 Evgenii Lepikhin <johnlepikhin@gmail.com>
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

(define-module (johnlepikhin packages i3im)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix build-system cargo)
  #:use-module ((guix build-system cargo) #:select (cargo-inputs)))

(define-public i3im
  (package
    (name "i3im")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri name version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0wrfjjgi6ripfyqj4d36iaygfq8dnzicfjnyy92jdlily7zr82b3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f))
    (native-inputs (list pkg-config))
    (inputs
     (cargo-inputs 'i3im #:module '(johnlepikhin packages rust-crates)))
    (home-page "https://github.com/johnlepikhin/i3im")
    (synopsis "@command{i3im} i3 IMproved")
    (description
     "The utility extends the standard functionality of the i3 and Sway window managers.")
    (license (list license:expat))))