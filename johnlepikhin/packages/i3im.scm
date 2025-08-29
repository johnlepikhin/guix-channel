;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Evgenii Lepikhin <johnlepikhin@gmail.com>
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
  #:use-module (gnu packages pkg-config)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public i3im
  (package
    (name "i3im")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri name version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1h7a0jcma3fjqi8f4l0crdl67yqaypd0b1d6f81c84gjzm4j3rp3"))
       (modules '((guix build utils)))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f
       ;; Cargo dependencies will be resolved automatically by cargo
       #:install-source? #t))
    (native-inputs (list pkg-config))
    (home-page "https://github.com/johnlepikhin/i3im")
    (synopsis "@command{i3im} i3 IMproved")
    (description
     "The utility extends the standard functionality of the i3 and Sway window managers.")
    (license (list license:expat))))