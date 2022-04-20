(define-module (johnlepikhin home ssh)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (srfi srfi-1)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:export (home-ssh-host-configuration
            home-ssh-configuration
            home-ssh-service-type))

(define-record-type* <home-ssh-host-configuration>
  home-ssh-host-configuration make-home-ssh-host-configuration
  home-ssh-host-configuration?
  (host-mask home-ssh-host-configuration-host-mask)
  (hostname home-ssh-host-configuration-hostname (default #f))
  (port home-ssh-host-configuration-port (default #f))
  (user home-ssh-host-configuration-user (default #f))
  (compression home-ssh-host-configuration-compression (default #f))
  (server-alive-interval home-ssh-host-configuration-server-alive-interval (default #f))
  (connect-timeout home-ssh-host-configuration-connect-timeout (default #f))
  (control-master home-ssh-host-configuration-control-master (default #f))
  (control-persist home-ssh-host-configuration-control-persist (default #f))
  (proxy-jump home-ssh-host-configuration-proxy-jump (default #f))
  (pubkey-accepted-key-types home-ssh-host-configuration-pubkey-accepted-key-types (default #f))
  (host-key-algorithms home-ssh-host-configuration-host-key-algorithms (default #f))
  (comment home-ssh-host-configuration-comment (default #f)))

(define (serialize-home-ssh-host-configuration val)
  (string-append
   (if (home-ssh-host-configuration-comment val)
       (format #f "# ~a\n" (home-ssh-host-configuration-comment val)) "")
   (format #f "Host ~a\n" (home-ssh-host-configuration-host-mask val))
   (if (home-ssh-host-configuration-hostname val)
       (format #f "  Hostname ~a\n" (home-ssh-host-configuration-hostname val)) "")
   (if (home-ssh-host-configuration-port val)
       (format #f "  Port ~d\n" (home-ssh-host-configuration-port val)) "")
   (if (home-ssh-host-configuration-user val)
       (format #f "  User ~a\n" (home-ssh-host-configuration-user val)) "")
   (if (home-ssh-host-configuration-compression val)
       (format #f "  Compression yes\n") "")
   (if (home-ssh-host-configuration-server-alive-interval val)
       (format #f "  ServerAliveInterval ~d\n" (home-ssh-host-configuration-server-alive-interval val)) "")
   (if (home-ssh-host-configuration-connect-timeout val)
       (format #f "  ConnectTimeout ~d\n" (home-ssh-host-configuration-connect-timeout val)) "")
   (if (home-ssh-host-configuration-control-master val)
       (format #f "  ControlMaster ~a\n" (home-ssh-host-configuration-control-master val)) "")
   (if (home-ssh-host-configuration-control-persist val)
       (format #f "  ControlPersist yes\n") "")
   (if (home-ssh-host-configuration-proxy-jump val)
       (format #f "  ProxyJump ~a\n" (home-ssh-host-configuration-proxy-jump val)) "")
   (if (home-ssh-host-configuration-pubkey-accepted-key-types val)
       (format #f "  PubkeyAcceptedKeyTypes ~a\n" (home-ssh-host-configuration-pubkey-accepted-key-types val)) "")
   (if (home-ssh-host-configuration-host-key-algorithms val)
       (format #f "  HostKeyAlgorithms ~a\n" (home-ssh-host-configuration-host-key-algorithms val)) "")
   "\n"))

(define-record-type* <home-ssh-configuration>
  home-ssh-configuration make-home-ssh-configuration
  home-ssh-configuration?
  (hosts home-ssh-configuration-hosts (default '())))

(define (ssh-control-path _)
  (string-append (getenv "XDG_CACHE_HOME") "/ssh-connections"))

(define (add-ssh-config-file config)
  `((".ssh/config"
     ,(mixed-text-file
       "ssh-config"
       #~(string-append
          #$@(append
              (list "ControlPath " (ssh-control-path '()) "/%h:%p:%r\n\n")
              (map (lambda (host)
                     (serialize-home-ssh-host-configuration host))
                   (home-ssh-configuration-hosts config))))))))

(define (add-ssh-extensions config extensions)
  (home-ssh-configuration
   (inherit config)
   (hosts
    (append (home-ssh-configuration-hosts config)
            extensions))))

(define (home-ssh-activation config)
  (let ((path (ssh-control-path '())))
    #~(begin
        (format #t "Creating ~a for persistent ssh control path\n" #$path)
        (mkdir-p #$path))))

(define home-ssh-service-type
  (service-type (name 'home-ssh)
                (extensions
                 (list
                  (service-extension home-files-service-type add-ssh-config-file)
                  (service-extension home-activation-service-type home-ssh-activation)))
                (compose concatenate)
                (extend add-ssh-extensions)
                (description "Create @file{~/.ssh/config}")))
