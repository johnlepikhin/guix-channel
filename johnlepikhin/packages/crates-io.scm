;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Evgenii Lepikhin <johnlepikhin@gmail.com>
;;;
;;; This file is part of GNU Guix.
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

(define-module (johnlepikhin packages crates-io)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages python)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages base)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages version-control)
  #:use-module (johnlepikhin packages rust-binary))

(define-public uv-bin
  (package
    (name "uv-bin")
    (version "0.7.20")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/astral-sh/uv/releases/download/"
                          version "/uv-x86_64-unknown-linux-gnu.tar.gz"))
       (sha256
        (base32 "0rh67y3sjb4f3cp724s5sbaai46i201isg5549fr527idx109whh"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       '(("uv" "bin/")
         ("uvx" "bin/"))
       #:validate-runpath? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'patch-elf
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (patchelf (string-append
                               (assoc-ref inputs "patchelf") "/bin/patchelf"))
                    (ld-so (string-append
                            (assoc-ref inputs "glibc") "/lib/ld-linux-x86-64.so.2"))
                    (gcc-lib (string-append
                              (assoc-ref inputs "gcc:lib") "/lib")))
               (invoke patchelf "--set-interpreter" ld-so
                       (string-append out "/bin/uv"))
               (invoke patchelf "--set-interpreter" ld-so
                       (string-append out "/bin/uvx"))
               (invoke patchelf "--set-rpath" gcc-lib
                       (string-append out "/bin/uv"))
               (invoke patchelf "--set-rpath" gcc-lib
                       (string-append out "/bin/uvx"))
               #t))))))
    (inputs
     `(("gcc:lib" ,gcc "lib")
       ("glibc" ,glibc)))
    (native-inputs
     `(("patchelf" ,patchelf)))
    (home-page "https://github.com/astral-sh/uv")
    (synopsis "Extremely fast Python package and project manager (binary release)")
    (description
     "uv is an extremely fast Python package and project manager, written in
Rust.  It is designed to be a drop-in replacement for pip and pip-tools
workflows.  This is the pre-built binary release.")
    (supported-systems '("x86_64-linux"))
    (license (list license:expat license:asl2.0))))

(define-public ruff-bin
  (package
    (name "ruff-bin")
    (version "0.12.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/astral-sh/ruff/releases/download/"
                          version "/ruff-x86_64-unknown-linux-gnu.tar.gz"))
       (sha256
        (base32 "0xyr769bipa4m500qylq87k0fp3chh8kmazdlv9v0hj7gzmv0dzb"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       '(("ruff" "bin/"))
       #:validate-runpath? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'patch-elf
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (patchelf (string-append
                               (assoc-ref inputs "patchelf") "/bin/patchelf"))
                    (ld-so (string-append
                            (assoc-ref inputs "glibc") "/lib/ld-linux-x86-64.so.2"))
                    (gcc-lib (string-append
                              (assoc-ref inputs "gcc:lib") "/lib")))
               (invoke patchelf "--set-interpreter" ld-so
                       (string-append out "/bin/ruff"))
               (invoke patchelf "--set-rpath" gcc-lib
                       (string-append out "/bin/ruff"))
               #t))))))
    (inputs
     `(("gcc:lib" ,gcc "lib")
       ("glibc" ,glibc)))
    (native-inputs
     `(("patchelf" ,patchelf)))
    (home-page "https://github.com/astral-sh/ruff")
    (synopsis "Extremely fast Python linter and formatter (binary release)")
    (description
     "Ruff is an extremely fast Python linter and code formatter, written in
Rust.  It can be used to replace Black, isort, Flake8, PyLint, pyupgrade,
autoflake, and more.  This is the pre-built binary release.")
    (supported-systems '("x86_64-linux"))
    (license license:expat)))
