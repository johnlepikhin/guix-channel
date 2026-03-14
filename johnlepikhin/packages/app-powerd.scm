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

(define-module (johnlepikhin packages app-powerd)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system cargo)
  #:use-module (johnlepikhin packages rust-binary))

(define-public app-powerd
  (package
    (name "app-powerd")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri name version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "086m4ba5g9ivinnqqicswd7k3cwhw2mz6a2kijdj6rh3hqz0mh7d"))))
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
    (inputs (append (list libxcb)
                    (cargo-inputs 'app-powerd
                                  #:module '(johnlepikhin packages rust-crates))))
    (home-page "https://github.com/johnlepikhin/app-powerd")
    (synopsis "Daemon that saves battery by freezing/throttling unfocused GUI apps")
    (description
     "app-powerd is a user-level Linux daemon that automatically manages
background GUI applications through cgroup v2 freeze and CPU throttling
to save battery power.")
    (license license:expat)))
