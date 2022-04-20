(define-module (johnlepikhin home xresources)
  #:use-module (gnu home services)
  #:use-module (srfi srfi-1)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (gnu home services shells)
  #:export (home-xresources-service-type
            home-xresources-configuration
            home-xresources-record))

(define-record-type* <home-xresources-record>
  home-xresources-record make-xresources-record
  home-xresources-record?
  (name home-xresources-record-name)
  (value home-xresources-record-value)
  (comment home-xresources-record-comment (default #f)))

(define-record-type* <home-xresources-configuration>
  home-xresources-configuration make-xresources-configuration
  home-xresources-configuration?
  (records home-xresources-configuration-records (default '())))

(define (serialize-home-xresources-record record)
  (string-append
   (if (home-xresources-record-comment record)
       (format #f "! ~a\n" (home-xresources-record-comment record)) "")
   (format #f "~a: ~a\n"
           (home-xresources-record-name record)
           (home-xresources-record-value record))))

(define (add-xresources-file config)
  `((".config/xresources"
     ,(mixed-text-file
       "config-xresources"
       #~(string-append
          #$@(map
              serialize-home-xresources-record
              (home-xresources-configuration-records config)))))))

(define (add-xresources-extensions config extensions)
  (home-xresources-configuration
   (inherit config)
   (records
    (append (home-xresources-configuration-records config)
            extensions))))

(define (add-xresources-to-shell-profile config)
  (list (plain-file "xrdb-merge-xresources.sh" (string-append "xrdb -merge <" (getenv "HOME") "/.config/xresources"))))

(define (home-xresources-activation config)
  `(("files/xresources" ,#~(system #$(string-append "xrdb -merge <" (getenv "HOME") "/.config/xresources")))))

(define home-xresources-service-type
  (service-type
   (name 'home-xresources)
   (extensions
    (list
     (service-extension home-files-service-type add-xresources-file)
     (service-extension home-shell-profile-service-type add-xresources-to-shell-profile)
     (service-extension home-run-on-change-service-type home-xresources-activation)))
   (compose concatenate)
   (extend add-xresources-extensions)
   (description "Creates @file{~/.config/xresources} and configures autoloading for it")))
