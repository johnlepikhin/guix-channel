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
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module (guix git)
  #:use-module (guix packages))

(define-public rust-xcb-0
  (package
    (name "rust-xcb")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "xcb" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "19i2pm8alpn2f0m4jg8bsw6ckw8irj1wjh55h9pi2fcb2diny1b2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-build-flags '("--all-features")
       #:cargo-test-flags '("--all-features")
       #:cargo-development-inputs (("rust-gl" ,rust-gl-0.14)
                                   ("rust-png" ,rust-png-0.17)
                                   ("rust-x11" ,rust-x11-2))
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-quick-xml" ,rust-quick-xml-0.22)
                       ("rust-x11" ,rust-x11-2))))
    (inputs (list mesa)) ;required by rust-x11-2
    (native-inputs (list pkg-config))
    (home-page "https://github.com/rust-x-bindings/rust-xcb")
    (synopsis "Rust bindings and wrappers for XCB")
    (description "This package provides Rust bindings and wrappers for XCB.")
    (license license:expat)))

(define-public rust-xidlehook-core-0
  (package
    (name "rust-xidlehook-core")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "xidlehook-core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "03p9fksj88z93lc3wqcvhz5c69zmznljzvg98dkljc51xp4bk0zh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs (("rust-gl" ,rust-gl-0.14)
                                   ("rust-png" ,rust-png-0.17)
                                   ("rust-x11" ,rust-x11-2))
       #:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-quick-xml" ,rust-quick-xml-0.22)
                       ("rust-x11" ,rust-x11-2))))
    (native-inputs (list pkg-config))
    (home-page "https://github.com/jD91mZM2/xidlehook")
    (synopsis "xidlehook core library")
    (description "xidlehook core library")
    (license license:expat)))

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
     `(#:cargo-inputs (("rust-env-logger" ,rust-env-logger-0.9)
                       ("rust-nix" ,rust-nix-0.15)
                       ("rust-structopt" ,rust-structopt-0.3)
                       ("rust-xcb" ,rust-xcb-0)
                       ("rust-xidlehook-core" ,rust-xidlehook-core-0)
                       ("rust-libpulse-binding" ,rust-libpulse-binding-2))))
    (inputs (list mesa pulseaudio))
    (native-inputs (list pkg-config python))
    (home-page "https://github.com/jD91mZM2/xidlehook")
    (synopsis
     "@command{xidlehook} is a general-purpose replacement for xautolock.")
    (description
     "Because xautolock is annoying to work with.

xidlehook is a general-purpose replacement for xautolock. It executes a command when the computer has been idle for a specified amount of time.")
    (license (list license:expat))))
