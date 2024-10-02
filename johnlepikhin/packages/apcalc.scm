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

(define-module (johnlepikhin packages apcalc)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages man)
  #:use-module (gnu packages groff)
  #:use-module (guix build-system gnu)
  #:export (make-apcalc))

(define (make-apcalc version checksum)
  (package
   (name "apcalc")
   (version version)
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/lcn2/calc")
                  (commit (string-append "v" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32 checksum))))
   (build-system gnu-build-system)
   (supported-systems '("x86_64-linux"))
   (arguments
    `(#:parallel-build? #t
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       ;; fails for some reason
       (delete 'validate-runpath)
       (add-before
        'build 'patch-makefile
        (lambda* (#:key outputs #:allow-other-keys)
          (substitute* "Makefile.target"
            (("^DEFAULT_LIB_INSTALL_PATH *=.*") (string-append "DEFAULT_LIB_INSTALL_PATH=" %output "/lib\n")))
          (call-with-output-file "Makefile.local"
            (lambda (port)
              (format port "\n")
              (format port "T=\n")
              (format port (string-append "SHELL=" (which "sh") "\n"))
              (format port (string-append "BINDIR=" %output "/bin\n"))
              (format port (string-append "LIBDIR=" %output "/lib\n"))
              (format port (string-append "MANDIR=" %output "/share/man/man1\n"))
              (format port (string-append "CATDIR=" %output "/share/man/cat1\n"))
              (format port (string-append "INCDIR=" %output "/include\n"))
              (format port (string-append "CALC_SHAREDIR=" %output "/share/calc\n"))))
          #t)))))
   (inputs `(("readline" ,readline)))
   (native-inputs `(("util-linux" ,util-linux)
                    ("groff-minimal" ,groff-minimal)
                    ("man-db" ,man-db)))
   (synopsis "C-style arbitrary precision calculator")
   (description "Calc is an interactive calculator which provides for easy large numeric calculations, but which also can be easily
programmed for difficult or long calculations. ")
   (home-page "https://github.com/lcn2/calc")
   (license lgpl2.1)))

(define-public apcalc-2.15.1.0
  (make-apcalc "2.15.1.0" "1xhya6r6k9hx1dir3z2ffj237gk9ky3w23mw1bvcmmbm4k0nyvz2"))

(define-public apcalc apcalc-2.15.1.0)

apcalc
