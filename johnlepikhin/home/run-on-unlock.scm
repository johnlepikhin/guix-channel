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

(define-module (johnlepikhin home run-on-unlock)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (johnlepikhin home xsession)
  #:use-module (srfi srfi-1)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:export (home-run-on-unlock-configuration
            home-run-on-unlock-service-type
            home-run-on-unlock-script-path))

(define-record-type* <home-run-on-unlock-configuration>
  home-run-on-unlock-configuration make-run-on-unlock-configuration
  home-run-on-unlock-configuration?
  (components home-run-on-unlock-components (default '())))

(define home-run-on-unlock-script-path ".local/bin/run-on-xsession-unlock")

(define (add-run-on-unlock-file config)
  `((,home-run-on-unlock-script-path
     ,(computed-file
       "run-on-unlock"
       (gexp
        (begin
          (with-output-to-file
              #$output
            (lambda _ (display
                       (string-append
                        "#! /bin/sh\n\n"
                        ". ~/.profile\n"
                        #$@(map (lambda (component) (string-append component "\n"))
                                (home-run-on-unlock-components config))))))
          (chmod #$output #o755)))))))

(define (add-run-on-unlock-extensions config extensions)
  (home-run-on-unlock-configuration
   (inherit config)
   (components (append (home-run-on-unlock-components config) extensions))))

(define (add-xsession-component config)
  home-run-on-unlock-script-path)

(define home-run-on-unlock-service-type
  (service-type
   (name 'home-run-on-unlock)
   (extensions
    (list
     (service-extension home-xsession-service-type add-xsession-component)
     (service-extension home-files-service-type add-run-on-unlock-file)))
   (compose identity)
   (extend add-run-on-unlock-extensions)
   (default-value (home-run-on-unlock-configuration))
   (description (string-append "List of commands to run after unlocking X xsession.scm

Currently only @file{~/ "
                               home-run-on-unlock-script-path
                               "} is created and all further integrations should be done manually."))))
