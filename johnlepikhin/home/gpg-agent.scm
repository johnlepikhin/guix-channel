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

(define-module (johnlepikhin home gpg-agent)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (srfi srfi-1)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:export (home-gpg-agent-configuration
            home-gpg-agent-service-type))

(define-record-type* <home-gpg-agent-configuration>
  home-gpg-agent-configuration make-home-gpg-agent-configuration
  home-gpg-agent-configuration?
  (default-cache-ttl home-gpg-agent-configuration-default-cache-ttl (default 600))
  (max-cache-ttl home-gpg-agent-configuration-max-cache-ttl (default 7200)))

(define (serialize-home-gpg-agent-configuration configuration)
  (string-append
   (format #f "default-cache-ttl ~a\n" (home-gpg-agent-configuration-default-cache-ttl configuration))
   (format #f "max-cache-ttl ~a\n" (home-gpg-agent-configuration-max-cache-ttl configuration))
   (format #f "pinentry-program ~a\n" (file-append pinentry "/bin/pinentry"))))

(define (add-gpg-agent-file config)
  `((".gnupg/gpg-agent.conf"
     ,(mixed-text-file
       "gnupg-gpg-agent.conf"
       (serialize-home-gpg-agent-configuration config)))))

(define (add-packages config)
  (list (gnupg pinentry)))

(define home-gpg-agent-service-type
  (service-type
   (name 'home-gpg-agent)
   (extensions
    (list
     (service-extension
      home-profile-service-type add-packages)))
     (service-extension home-files-service-type add-gpg-agent-file)))
   (compose concatenate)
   (description "Create @file{~/.gnupg/gpg-agent.conf}")))
