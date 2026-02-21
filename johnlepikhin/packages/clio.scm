;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Evgenii Lepikhin <johnlepikhin@gmail.com>
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

(define-module (johnlepikhin packages clio)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages rust-crates)
  #:use-module (guix build-system cargo)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (johnlepikhin packages rust-binary)
  #:use-module (johnlepikhin packages rust-crates))

(define-public clio
  (package
    (name "clio")
    (version "0.3.2")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/johnlepikhin/clio.git")
                   (commit (string-append "v" version))))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "1q0q1k6yr8nc9g7li9cx8izyfqm7cnbhh3mwm5jrln8sxwdsmha2"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:rust rust-binary-1.88
      #:install-source? #f
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'create-cc-symlink
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((gcc (assoc-ref inputs "gcc-toolchain")))
                (mkdir-p "/tmp/bin")
                (symlink (string-append gcc "/bin/gcc") "/tmp/bin/cc")
                (setenv "PATH" (string-append "/tmp/bin:" (getenv "PATH")))
                (setenv "CC" (string-append gcc "/bin/gcc"))
                (setenv "HOST_CC" (string-append gcc "/bin/gcc"))))))))
    (native-inputs (list gcc-toolchain pkg-config))
    (inputs (append (list bash-minimal gtk sqlite)
                    (cargo-inputs 'clio
                                  #:module '(johnlepikhin packages rust-crates))))
    (home-page "https://github.com/johnlepikhin/clio")
    (synopsis "Clipboard manager with SQLite history and GTK4 UI")
    (description "Clio is a clipboard manager for Linux.  It monitors the clipboard,
stores history in SQLite, and provides a GTK4 UI for browsing and searching
clipboard entries.")
    (license license:expat)))
