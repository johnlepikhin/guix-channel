;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025, 2026 Evgenii Lepikhin <johnlepikhin@gmail.com>
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

(define-module (johnlepikhin packages python-xyz)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages base)
  #:use-module (gnu packages elf))

(define-public uv-bin
  (package
    (name "uv-bin")
    (version "0.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/astral-sh/uv/releases/download/"
                          version "/uv-x86_64-unknown-linux-gnu.tar.gz"))
       (sha256
        (base32 "1fxwni0n2i9xffm311cq2ws5jy29yq0scz8ikss9vlb9zciz5clh"))))
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
    (version "0.16.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/astral-sh/ruff/releases/download/"
                          version "/ruff-x86_64-unknown-linux-gnu.tar.gz"))
       (sha256
        (base32 "0mjc1bkh25rp60g8zpblmhpq4zcrv0pk0xn0k0nbn4a9varwj73y"))))
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