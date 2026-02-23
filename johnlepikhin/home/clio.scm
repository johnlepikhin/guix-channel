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

(define-module (johnlepikhin home clio)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages suckless)
  #:use-module (johnlepikhin home xsession)
  #:use-module (johnlepikhin packages clio)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:export (home-clio-configuration
            home-clio-service-type))

(define-record-type* <home-clio-configuration>
  home-clio-configuration make-home-clio-configuration
  home-clio-configuration?
  (package home-clio-configuration-package (default clio))
  (passmenu? home-clio-configuration-passmenu? (default #t))
  (passmenu-ttl home-clio-configuration-passmenu-ttl (default "30s")))

(define (add-clio-package config)
  (list (home-clio-configuration-package config)))

(define (add-xsession-component config)
  "clio watch &")

(define (make-passmenu-clio-script config)
  (let ((clio-pkg (home-clio-configuration-package config))
        (ttl (home-clio-configuration-passmenu-ttl config)))
    (computed-file
     "passmenu-clio"
     (gexp
      (begin
        (with-output-to-file
            #$output
          (lambda _
            (display
             (string-append
              "#!" #$(file-append bash-minimal "/bin/bash") "\n"
              "\n"
              "shopt -s nullglob globstar\n"
              "\n"
              "dmenu=" #$(file-append dmenu "/bin/dmenu") "\n"
              "pass=" #$(file-append password-store "/bin/pass") "\n"
              "clio=" #$(file-append clio-pkg "/bin/clio") "\n"
              "\n"
              "prefix=${PASSWORD_STORE_DIR-~/.password-store}\n"
              "password_files=( \"$prefix\"/**/*.gpg )\n"
              "password_files=( \"${password_files[@]#\"$prefix\"/}\" )\n"
              "password_files=( \"${password_files[@]%.gpg}\" )\n"
              "\n"
              "password=$(printf '%s\\n' \"${password_files[@]}\" | \"$dmenu\" \"$@\")\n"
              "\n"
              "[[ -n $password ]] || exit\n"
              "\n"
              "\"$pass\" show \"$password\" | head -1 | "
              "\"$clio\" copy --ttl " #$ttl " --mask-with 'Secret from pass'\n"))))
        (chmod #$output #o755))))))

(define (add-passmenu-clio-file config)
  (if (home-clio-configuration-passmenu? config)
      `((".local/bin/passmenu-clio"
         ,(make-passmenu-clio-script config)))
      '()))

(define home-clio-service-type
  (service-type
   (name 'home-clio)
   (extensions
    (list
     (service-extension home-profile-service-type add-clio-package)
     (service-extension home-xsession-service-type add-xsession-component)
     (service-extension home-files-service-type add-passmenu-clio-file)))
   (default-value (home-clio-configuration))
   (description "Install clio and start clipboard watcher in xsession")))
