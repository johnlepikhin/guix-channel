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

(define-module (johnlepikhin devel python)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (gnu packages python)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-xyz)
  #:use-module (johnlepikhin packages crates-io)
  #:use-module (srfi srfi-1)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:export (home-devel-python-configuration
            home-devel-python-service-type))

(define-record-type* <home-devel-python-configuration>
  home-devel-python-configuration make-home-devel-python-configuration
  home-devel-python-configuration?)

(define (add-packages config)
  (list
   python
   python-black
   python-flake8
   python-flake8-bugbear
   python-isort
   python-language-server
   python-mccabe
   python-mypy
   python-pep8-naming
   python-pylint
   python-pytest-flake8
   python-virtualenv
   uv-bin
   ruff-bin))

(define home-devel-python-service-type
  (service-type
   (name 'home-devel-python)
   (extensions
    (list
     (service-extension home-profile-service-type
                        add-packages)))
   (compose concatenate)
   (description "Install packages required for Python development")))
