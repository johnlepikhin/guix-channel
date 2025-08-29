;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Evgenii Lepikhin <johnlepikhin@gmail.com>
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

(define-module (johnlepikhin packages convco)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public convco
  (package
    (name "convco")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri name version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1m8si96p1m60wg93syf3f77ksn0mg5cbd19n4n8hxn3n2ix9clhr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       ;; Disable default features which include zlib-ng
       #:cargo-build-flags '("--release" "--no-default-features")
       ;; Tests require zlib-ng, disable them
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-env-vars
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Ensure sys crates use system libraries
             (setenv "LIBZ_SYS_STATIC" "0")
             (setenv "LIBGIT2_NO_VENDOR" "1")
             (setenv "LIBSSH2_SYS_USE_PKG_CONFIG" "1")
             (setenv "LIBGIT2_SYS_USE_PKG_CONFIG" "1")
             (setenv "OPENSSL_NO_VENDOR" "1")
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (invoke "cargo" "install" "--no-track"
                       "--path" "." "--root" out
                       "--no-default-features")))))))
    (native-inputs (list pkg-config))
    (inputs (list git-minimal
                  zlib
                  libgit2-1.9
                  libssh2
                  openssl))
    (home-page "https://convco.github.io/")
    (synopsis "Conventional commit CLI tool")
    (description
     "Convco is a CLI tool that helps you to follow conventional commits.
It provides helpful features like conventional commit templates, automatic
changelog generation, and version number updates.")
    (license (list license:expat))))