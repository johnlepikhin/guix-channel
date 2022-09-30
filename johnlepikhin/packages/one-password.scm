;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Evgenii Lepikhin <johnlepikhin@gmail.com>
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

(define-module (johnlepikhin packages one-password)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (gnu packages base)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages compression)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy)
  #:export (make-1password))

(define license:1password
  (non-copyleft "Proprietary 1Password license"
           "https://1password.com/ru/legal/terms-of-service/"))

(define (make-1password-cli version checksum)
  (package
    (name "1password")
    (version version)
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append "https://cache.agilebits.com/dist/1P/op/pkg/v" version "/op_linux_amd64_v" version ".zip"))
              (sha256 (base32 checksum))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       `(("op" "bin/1password"))
       #:phases
       (modify-phases
        %standard-phases
        (add-after
         'strip 'fix-binary
         (lambda*
          (#:key outputs inputs #:allow-other-keys)
          (let* ((out (assoc-ref outputs "out"))
                 (patchelf (string-append (assoc-ref inputs "patchelf") "/bin/patchelf"))
                 (binary (string-append out "/bin/1password"))
                 (dynamic-linker (string-append (assoc-ref inputs "libc") ,(glibc-dynamic-linker))))
            (system (string-append patchelf " --set-interpreter " dynamic-linker " " binary))))))))
    (synopsis "1Password CLI")
    (description "1Password CLI")
    (home-page "https://1password.com/")
    (native-inputs `(("patchelf" ,patchelf)))
    (inputs `(("glibc" ,glibc)))
    (license license:1password)))

(define-public 1password-cli-1.10.0 (make-1password-cli "1.10.0" "0zj30fwjgj0fv16z24jq0pfsqf3q7ckbfp8rpwaqw7a88b8z4nx5"))

(define-public 1password-cli 1password-cli-1.10.0)
