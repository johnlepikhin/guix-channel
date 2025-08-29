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

(define-module (johnlepikhin packages xidlehook)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module (guix packages))

(define-public xidlehook
  (package
    (name "xidlehook")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri name version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1p4agz37fvwzrhl9zywgb5p375ljr6j01h34ijyzzic8i2zrkm97"))
       (modules '((guix build utils)))
       (snippet '(substitute* "Cargo.toml"
                   (("^readme = .*")
                    "")))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f
       ;; Cargo dependencies will be resolved automatically by cargo
       #:install-source? #t))
    (inputs (list mesa pulseaudio))
    (native-inputs (list pkg-config python))
    (home-page "https://github.com/jD91mZM2/xidlehook")
    (synopsis
     "@command{xidlehook} is a general-purpose replacement for xautolock.")
    (description
     "Because xautolock is annoying to work with.

xidlehook is a general-purpose replacement for xautolock. It executes a command when the computer has been idle for a specified amount of time.")
    (license (list license:expat))))