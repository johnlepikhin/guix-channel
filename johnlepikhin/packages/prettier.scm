;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Evgenii Lepikhin <johnlepikhin@gmail.com>
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

(define-module (johnlepikhin packages prettier)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages node))

(define-public prettier
  (package
    (name "prettier")
    (version "3.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://registry.npmjs.org/prettier/-/prettier-"
                           version ".tgz"))
       (sha256
        (base32 "1psvhwxg27w32lscypsavl3jbv28dxw17l5p0635l5sgcy1sp0dw"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan '(("." "lib/node_modules/prettier"))
       #:phases (modify-phases %standard-phases
                  (add-after 'install 'install-bin
                    (lambda* (#:key outputs inputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (bin (string-append out "/bin"))
                             (node (assoc-ref inputs "node")))
                        (mkdir-p bin)
                        (with-output-to-file (string-append bin "/prettier")
                          (lambda ()
                            (display (string-append "#!"
                                                    (which "sh") "\n"))
                            (display (string-append "exec "
                                                    node
                                                    "/bin/node "
                                                    out
                                                    "/lib/node_modules/prettier/bin/prettier.cjs"
                                                    " \"$@\"\n"))))
                        (chmod (string-append bin "/prettier") #o755)
                        #t))))))
    (inputs (list node))
    (home-page "https://prettier.io")
    (synopsis "Opinionated code formatter")
    (description
     "Prettier is an opinionated code formatter.  It enforces a consistent
style by parsing your code and re-printing it with its own rules that take
the maximum line length into account, wrapping code when necessary.")
    (license license:expat)))