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

(define-module (johnlepikhin home claude-dunst)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages web)
  #:use-module (guix gexp)
  #:export (home-claude-dunst-service-type))

(define (add-files config)
  `((".local/bin/claude-dunst-notify"
     ,(local-file "files/claude-dunst-notify" #:recursive? #t))))

(define (add-packages config)
  (list dunst jq))

(define home-claude-dunst-service-type
  (service-type
   (name 'home-claude-dunst)
   (extensions
    (list
     (service-extension home-profile-service-type add-packages)
     (service-extension home-files-service-type add-files)))
   (default-value #f)
   (description "Setup Claude Code desktop notifications via dunstify with jq dependency.")))
