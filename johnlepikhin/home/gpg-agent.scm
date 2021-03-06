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
   (format #f "max-cache-ttl ~a\n" (home-gpg-agent-configuration-max-cache-ttl configuration))))

(define (add-gpg-agent-file config)
  `((".gnupg/gpg-agent.conf"
     ,(mixed-text-file
       "gnupg-gpg-agent.conf"
       (serialize-home-gpg-agent-configuration config)))))

(define home-gpg-agent-service-type
  (service-type
   (name 'home-gpg-agent)
   (extensions
    (list
     (service-extension home-files-service-type add-gpg-agent-file)))
   (compose concatenate)
   (description "Create @file{~/.gnupg/gpg-agent.conf}")))
