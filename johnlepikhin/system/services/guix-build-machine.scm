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

;;; The build-machine side of `guix offload': an account the offloading
;;; daemons log in as over SSH, and trust in their signing keys so that the
;;; inputs they send are accepted.

(define-module (johnlepikhin system services guix-build-machine)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services ssh)
  #:use-module (gnu system shadow)
  #:use-module (guix records)
  #:export (guix-build-machine-configuration
            guix-build-machine-configuration?
            guix-build-machine-service-type))

(define-record-type* <guix-build-machine-configuration>
  guix-build-machine-configuration
  make-guix-build-machine-configuration
  guix-build-machine-configuration?
  (user          guix-build-machine-configuration-user
                 (default "guix-offload"))
  ;; SSH public keys (file-like) of the clients' root accounts: the
  ;; offloading guix-daemon connects as root's key from machines.scm.
  (ssh-keys      guix-build-machine-configuration-ssh-keys
                 (default '()))
  ;; Clients' /etc/guix/signing-key.pub (file-like).
  (signing-keys  guix-build-machine-configuration-signing-keys
                 (default '())))

(define (guix-build-machine-accounts config)
  (let ((user (guix-build-machine-configuration-user config)))
    (list (user-group (name user) (system? #t))
          (user-account
           (name user)
           (group user)
           (system? #t)
           (comment "guix offload")
           (home-directory (string-append "/var/lib/" user))
           ;; `guix offload' needs a shell to run `guix repl' in.
           (create-home-directory? #t)))))

(define (guix-build-machine-ssh-keys config)
  `((,(guix-build-machine-configuration-user config)
     ,@(guix-build-machine-configuration-ssh-keys config))))

(define (guix-build-machine-guix-extension config)
  (guix-extension
   (authorized-keys (guix-build-machine-configuration-signing-keys config))))

(define guix-build-machine-service-type
  (service-type
   (name 'guix-build-machine)
   (extensions
    (list (service-extension account-service-type
                             guix-build-machine-accounts)
          (service-extension openssh-service-type
                             guix-build-machine-ssh-keys)
          (service-extension guix-service-type
                             guix-build-machine-guix-extension)))
   (description "Let other machines offload builds to this one with
@command{guix offload}: create the account they log in as, authorize their SSH
keys for it and trust their store signing keys.")))
