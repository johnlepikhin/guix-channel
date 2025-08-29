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

(define-module (johnlepikhin packages shadowplay)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cargo)
  #:use-module (guix git-download)
  #:use-module (guix git)
  #:use-module (guix packages)
  #:use-module (gnu packages pkg-config)
  #:use-module ((johnlepikhin packages rust-crates) #:select (lookup-cargo-inputs)))

(define (make-shadowplay version)
  (package
    (name "shadowplay")
    (version version)
    (source (git-checkout
             (url "https://github.com/mailru/shadowplay")
             (commit (string-append "v" version))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f
       #:install-source? #f
       #:phases
       (modify-phases
           %standard-phases
         (add-after 'unpack 'permissions
           (lambda _
             (delete-file ".cargo/config.toml")
             #t)))))
    (inputs
     (lookup-cargo-inputs 'shadowplay))
    (native-inputs
     (list pkg-config))
    (home-page "https://gitlab.corp.mail.ru/MailBackendTest/devtools/shadowplay")
    (synopsis "@command{shadowplay} is Puppet linter, pretty printer and explorer")
    (description
     "Shadowplay is a utility that has the functionality of checking puppet syntax, a puppet manifest linter, a pretty printer, and a
utility for exploring the Hiera.")
    (license (list license:expat license:asl2.0))))

(define-public shadowplay-0.17.1 (make-shadowplay "0.17.1"))

(define-public shadowplay shadowplay-0.17.1)