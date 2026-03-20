;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Evgenii Lepikhin <johnlepikhin@gmail.com>
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

(define-module (johnlepikhin home tlp-shortcuts)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (guix gexp)
  #:export (home-tlp-shortcuts-service-type))

(define (add-files config)
  `((".local/bin/tlp-bat"
     ,(computed-file
       "bin-tlp-bat"
       #~(begin
           (with-output-to-file #$output
             (lambda _ (display "#!/bin/sh\nexec sudo tlp bat\n")))
           (chmod #$output #o755))))
    (".local/bin/tlp-ac"
     ,(computed-file
       "bin-tlp-ac"
       #~(begin
           (with-output-to-file #$output
             (lambda _ (display "#!/bin/sh\nexec sudo tlp ac\n")))
           (chmod #$output #o755))))))

(define home-tlp-shortcuts-service-type
  (service-type
   (name 'home-tlp-shortcuts)
   (extensions
    (list
     (service-extension home-files-service-type add-files)))
   (default-value #f)
   (description "Install TLP shortcut scripts for switching between battery and AC modes.")))
