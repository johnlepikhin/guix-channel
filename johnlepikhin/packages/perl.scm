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

(define-module (johnlepikhin packages perl)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system perl)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages language)
  #:use-module (gnu packages perl-check)
  #:export (perl-b-keywords))

(define-public perl-b-keywords
  (package
    (name "perl-b-keywords")
    (version "1.24")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RU/RURBAN/B-Keywords-"
                           version ".tar.gz"))
       (sha256
        (base32 "0i2ksp0w9wv1qc22hrdl3k48cww64syhmv8zf6x0kgyd4081hr56"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/B-Keywords")
    (synopsis "Lists of reserved barewords and symbol names")
    (description "@code{B::Keywords} supplies several arrays of exportable
keywords: @code{@@Scalars, @@Arrays, @@Hashes, @@Filehandles, @@Symbols,
@@Functions, @@Barewords, @@TieIOMethods, @@UNIVERSALMethods and
@@ExporterSymbols}.")
    ;; GPLv2 only
    (license license:gpl2)))
