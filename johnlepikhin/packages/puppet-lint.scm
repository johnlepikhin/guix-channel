;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022, 2026 Evgenii Lepikhin <johnlepikhin@gmail.com>
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

(define-module (johnlepikhin packages puppet-lint)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system ruby)
  #:use-module (gnu packages ruby)
  #:export (make-puppet-lint))

(define (make-puppet-lint version checksum)
  (package
    (name "puppet-lint")
    (version version)
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "puppet-lint" version))
              (sha256
               (base32 checksum))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))
    (synopsis "Puppet manifests linter")
    (description "Check that your Puppet manifest conform to the style guide")
    (home-page "http://puppet-lint.com")
    (license license:expat)))

(define-public puppet-lint-5.1.1
  (make-puppet-lint "5.1.1" "11j46i4bankyvmknl69pqz15ysrd8l6nwhbl77lslrswx7dlwxqy"))

(define-public puppet-lint puppet-lint-5.1.1)
