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

(define-module (johnlepikhin devel go)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-apps)
  #:use-module (johnlepikhin packages golangci-lint)
  #:use-module (srfi srfi-1)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:export (home-devel-go-configuration
            home-devel-go-service-type))

(define-record-type* <home-devel-go-configuration>
  home-devel-go-configuration make-home-devel-go-configuration
  home-devel-go-configuration?)

(define (add-packages config)
  (list
   go
   gopls
   golangci-lint))

(define (add-env-variables config)
  `(("GOPATH" . ,(string-append (getenv "HOME") "/go"))
    ("GOBIN" . ,(string-append (getenv "HOME") "/go/bin"))))

(define home-devel-go-service-type
  (service-type
   (name 'home-devel-go)
   (extensions
    (list
     (service-extension home-profile-service-type add-packages)
     (service-extension home-environment-variables-service-type add-env-variables)))
   (compose concatenate)
   (description "Install packages required for Go development")))
