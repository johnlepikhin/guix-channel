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

(define-module (johnlepikhin packages git-sync)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (gnu packages version-control)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:export (make-git-sync))

(define (make-git-sync version commit checksum)
  (package
    (name "git-sync")
    (version version)
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/simonthum/git-sync/")
                    (commit commit)))
             (file-name (git-file-name name version))
             (sha256
              (base32 checksum))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan `(("git-sync" "bin/git-sync"))))
    (synopsis "Synchronize tracking repositories")
    (description "This scrips intends to sync near-automatically via git in \"tracking\" repositories where a nice history is not as crucial
as having one.")
    (home-page "https://github.com/simonthum/git-sync/")
    (propagated-inputs `(("git" ,git)))
    (license cc0)))

(define-public git-sync-2022-04-10
  (make-git-sync "2022-04-10" "8466b77a38b3d5e8b4ed9e3cb1b635e475eeb415" "1bj8zq08272w30js5r43rcdndpn0ngi29kxw102hisymc6kb1c7j"))

(define-public git-sync git-sync-2022-04-10)
