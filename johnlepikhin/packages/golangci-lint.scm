;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Evgenii Lepikhin <johnlepikhin@gmail.com>
;;;
;;; This file is part of a Guix channel.
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define-module (johnlepikhin packages golangci-lint)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:))

(define-public golangci-lint
  (package
    (name "golangci-lint")
    (version "2.11.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/golangci/golangci-lint/releases/download/v"
             version "/golangci-lint-" version "-linux-amd64.tar.gz"))
       (sha256
        (base32 "1lbm0i2wfkm7vq22jbsfgwj6nln08qdsks34idvxa9f8pkfqrfw7"))))
    (supported-systems '("x86_64-linux"))
    (build-system gnu-build-system)
    (arguments
     `(#:strip-binaries? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (mkdir-p bin)
               (install-file "golangci-lint" bin)))))))
    (home-page "https://golangci-lint.run")
    (synopsis "Go linters aggregator")
    (description "@code{golangci-lint} aggregates Go linters and runs them
in parallel, reusing the Go build cache and caching analysis results
for significantly faster runs.  It includes over 100 linters and supports
@code{YAML}-based configuration.")
    (license license:gpl3)))
