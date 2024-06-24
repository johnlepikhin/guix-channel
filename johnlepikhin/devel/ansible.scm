
(define-module (johnlepikhin devel ansible)
  #:use-module (gnu home services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services)
  #:use-module (guix records)
  #:use-module (johnlepikhin packages ansible-lint)
  #:use-module (gnu packages admin)
  #:use-module (srfi srfi-1)
  #:export (home-devel-ansible-service-type
            home-devel-ansible-configuration))

(define-record-type* <home-devel-ansible-configuration>
  home-devel-ansible-configuration make-home-devel-ansible-configuration
  home-devel-ansible-configuration?
  (packages home-devel-ansible-configuration-packages
            (default (list ansible-lint))))

(define (add-packages config)
  (append
   (home-devel-ansible-configuration-packages config)
   (list ansible)))

(define home-devel-ansible-service-type
  (service-type
   (name 'home-devel-ansible)
   (extensions
    (list
     (service-extension home-profile-service-type add-packages)))
   (compose concatenate)
   (description "Install and configure Ansible for development")))
