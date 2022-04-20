(define-module (johnlepikhin home inputrc)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (srfi srfi-1)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:export (home-inputrc-record
            home-inputrc-configuration
            home-inputrc-service-type))

(define-record-type* <home-inputrc-record>
  home-inputrc-record make-home-inputrc-record
  home-inputrc-record?
  (key home-inputrc-record-key)
  (function home-inputrc-record-function)
  (comment home-inputrc-record-comment (default #f)))

(define (serialize-home-inputrc-record record)
  (string-append
   (if (home-inputrc-record-comment record)
       (format #f "# ~a\n" (home-inputrc-record-comment record)) "")
   (format #f "~a: ~a\n\n"
           (home-inputrc-record-key record)
           (home-inputrc-record-function record))))

(define-record-type* <home-inputrc-configuration>
  home-inputrc-configuration make-home-inputrc-configuration
  home-inputrc-configuration?
  (records home-inputrc-configuration-records (default '())))

(define (add-inputrc-file config)
  `((".inputrc"
     ,(mixed-text-file
       "inputrc"
       #~(string-append
          #$@(map (lambda (record)
                    (serialize-home-inputrc-record record))
                  (home-inputrc-configuration-records config)))))))

(define (add-inputrc-extensions config extensions)
  (home-inputrc-configuration
   (inherit config)
   (records
    (append (home-inputrc-configuration-records config) extensions))))

(define home-inputrc-service-type
  (service-type
   (name 'home-inputrc)
   (extensions
    (list
     (service-extension home-files-service-type add-inputrc-file)))
   (compose concatenate)
   (extend add-inputrc-extensions)
   (description "Create @file{~/.inputrc}")))
