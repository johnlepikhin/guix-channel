;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Evgenii Lepikhin <johnlepikhin@gmail.com>
;;;
;;; This file is part of GNU Guix.
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

(define-module (johnlepikhin packages ripsecrets)
  #:use-module (gnu packages rust-crates)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public ripsecrets
  (package
    (name "ripsecrets")
    (version "0.1.11")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ripsecrets" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0y95s0q7q7r23wshgg588r42ckipwdygh0n5ir7spb938c9khy60"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-dev-dependencies
           (lambda _
             ;; More precise removal of just the dev-dependencies section
             (substitute* "Cargo.toml"
               (("\\[dev-dependencies\\.criterion\\]") "# removed criterion")
               (("version = \"0\\.3\"") "# removed version")
               (("features = \\[\"html_reports\"\\]") "# removed features"))
             ;; Remove benches directory if it exists
             (when (file-exists? "benches")
               (delete-file-recursively "benches"))
             #t)))))
    (inputs (cargo-inputs 'ripsecrets #:module '(johnlepikhin packages rust-crates)))
    (home-page "https://github.com/sirwart/ripsecrets")
    (synopsis "Prevent committing secret keys into your source code")
    (description
     "ripsecrets is a command-line tool to prevent committing secret keys into
your source code.  It works by matching known secret patterns and detecting
random strings in variables with secret-related names.  The tool is designed
to be extremely fast and has a low false positive rate.")
    (license license:expat)))