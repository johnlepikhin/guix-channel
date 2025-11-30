;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Evgenii Lepikhin <johnlepikhin@gmail.com>
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

(define-module (johnlepikhin devel perl)
  #:use-module (gnu home services)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-web)
  #:use-module (gnu packages web)
  #:use-module (gnu services configuration)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (johnlepikhin packages perl)
  #:use-module (srfi srfi-1)
  #:export (home-devel-perl-configuration
            home-devel-perl-service-type))

(define-record-type* <home-devel-perl-configuration>
  home-devel-perl-configuration make-home-devel-perl-configuration
  home-devel-perl-configuration?
  (packages home-devel-perl-configuration-packages
            (default (list perl-file-slurp
                           perl-http-tiny
                           perl-json
                           perl-perlcritic
                           perl-uri-escape
                           perl-yaml-tiny
                           perl-tidy
                           perl-lwp-protocol-https
                           perl-libwww
                           perl-io-socket-ssl))))

(define (add-devel-perl-packages config)
  (append (home-devel-perl-configuration-packages config)
          (list perl)))

(define (add-env-variables config)
  `(("PERL5LIB" . ,(string-append (getenv "HOME") "/perl5/lib/perl5:$PERL5LIB"))))

(define (add-files config)
  `((".perlcriticrc" ,(local-file "files/perlcriticrc"))))

(define home-devel-perl-service-type
  (service-type
   (name 'home-devel-perl)
   (extensions
    (list
     (service-extension home-profile-service-type add-devel-perl-packages)
     (service-extension home-environment-variables-service-type add-env-variables)
     (service-extension home-files-service-type add-files)))
   (compose concatenate)
   (description "Install packages required for Perl development")))
