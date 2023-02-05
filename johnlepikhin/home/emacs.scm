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

(define-module (johnlepikhin home emacs)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages version-control)
  #:use-module (gnu home services)
  #:use-module (srfi srfi-1)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:export (home-emacs-configuration
            home-emacs-service-type))

(define-record-type* <home-emacs-configuration>
  home-emacs-configuration make-home-emacs-configuration
  home-emacs-configuration?
  (package home-emacs-configuration-package (default emacs))
  (configs-git-repo home-emacs-configuration-configs-git-repo))

(define (add-emacs-config config)
  `((".emacs" ,(local-file "files/emacs"))))

(define (add-emacs-package config)
  (list (home-emacs-configuration-package config)))

(define (git-clone-configs config)
  (gexp
   (begin
     (let ((clone-path (string-append (getenv "HOME") "/.emacs.d/public")))
       (mkdir-p clone-path)
       (system*
        (ungexp (file-append git "/bin/git"))
        "clone"
        (home-emacs-configuration-configs-git-repo config)
        clone-path)))))

(define home-emacs-service-type
  (service-type
   (name 'home-emacs)
   (extensions
    (list
     (service-extension
      home-files-service-type add-emacs-config)
     (service-extension
      home-profile-service-type add-emacs-package)
     (service-extension
      home-activation-service-type git-clone-configs)))
   (compose concatenate)
   (description "Install emacs and add configs")))
