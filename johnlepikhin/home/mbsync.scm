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

(define-module (johnlepikhin home mbsync)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages mail)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:export (home-mbsync-imap-store-configuration
            home-mbsync-imap-store-configuration?
            home-mbsync-maildir-store-configuration
            home-mbsync-maildir-store-configuration?
            home-mbsync-channel-configuration
            home-mbsync-channel-configuration?
            home-mbsync-group-configuration
            home-mbsync-group-configuration?
            home-mbsync-account-configuration
            home-mbsync-account-configuration?
            home-mbsync-configuration
            home-mbsync-configuration?
            home-mbsync-service-type))

;;;
;;; Record types
;;;

(define-record-type* <home-mbsync-imap-store-configuration>
  home-mbsync-imap-store-configuration make-home-mbsync-imap-store-configuration
  home-mbsync-imap-store-configuration?
  (name             home-mbsync-imap-store-configuration-name)
  (host             home-mbsync-imap-store-configuration-host)
  (user             home-mbsync-imap-store-configuration-user)
  (pass-cmd         home-mbsync-imap-store-configuration-pass-cmd
                    (default #f))
  (auth-mechs       home-mbsync-imap-store-configuration-auth-mechs
                    (default '()))
  (tls-type         home-mbsync-imap-store-configuration-tls-type
                    (default "IMAPS"))
  (certificate-file home-mbsync-imap-store-configuration-certificate-file
                    (default "/etc/ssl/certs/ca-certificates.crt")))

(define-record-type* <home-mbsync-maildir-store-configuration>
  home-mbsync-maildir-store-configuration make-home-mbsync-maildir-store-configuration
  home-mbsync-maildir-store-configuration?
  (name        home-mbsync-maildir-store-configuration-name)
  (path        home-mbsync-maildir-store-configuration-path)
  (inbox       home-mbsync-maildir-store-configuration-inbox)
  (sub-folders home-mbsync-maildir-store-configuration-sub-folders
               (default #f))
  (flatten     home-mbsync-maildir-store-configuration-flatten
               (default #f)))

(define-record-type* <home-mbsync-channel-configuration>
  home-mbsync-channel-configuration make-home-mbsync-channel-configuration
  home-mbsync-channel-configuration?
  (name       home-mbsync-channel-configuration-name)
  (far        home-mbsync-channel-configuration-far)
  (near       home-mbsync-channel-configuration-near)
  (patterns   home-mbsync-channel-configuration-patterns
              (default '()))
  (create     home-mbsync-channel-configuration-create
              (default "Both"))
  (expunge    home-mbsync-channel-configuration-expunge
              (default "None"))
  (sync-state home-mbsync-channel-configuration-sync-state
              (default "*")))

(define-record-type* <home-mbsync-group-configuration>
  home-mbsync-group-configuration make-home-mbsync-group-configuration
  home-mbsync-group-configuration?
  (name     home-mbsync-group-configuration-name)
  (channels home-mbsync-group-configuration-channels))

(define-record-type* <home-mbsync-account-configuration>
  home-mbsync-account-configuration make-home-mbsync-account-configuration
  home-mbsync-account-configuration?
  (name          home-mbsync-account-configuration-name)
  (imap-store    home-mbsync-account-configuration-imap-store)
  (maildir-store home-mbsync-account-configuration-maildir-store)
  (channels      home-mbsync-account-configuration-channels)
  (group         home-mbsync-account-configuration-group
                 (default #f)))

(define-record-type* <home-mbsync-configuration>
  home-mbsync-configuration make-home-mbsync-configuration
  home-mbsync-configuration?
  (package       home-mbsync-configuration-package
                 (default isync))
  (accounts      home-mbsync-configuration-accounts)
  (sync-interval home-mbsync-configuration-sync-interval
                 (default 180)))

;;;
;;; Serialization
;;;

(define (serialize-imap-store config)
  (let ((name (home-mbsync-imap-store-configuration-name config))
        (host (home-mbsync-imap-store-configuration-host config))
        (user (home-mbsync-imap-store-configuration-user config))
        (pass-cmd (home-mbsync-imap-store-configuration-pass-cmd config))
        (auth-mechs (home-mbsync-imap-store-configuration-auth-mechs config))
        (tls-type (home-mbsync-imap-store-configuration-tls-type config))
        (cert-file (home-mbsync-imap-store-configuration-certificate-file config)))
    (string-append
     "IMAPStore " name "\n"
     "Host " host "\n"
     "User " user "\n"
     (if pass-cmd
         (let ((escaped (string-join (string-split pass-cmd #\") "\\\"")))
          (string-append "PassCmd \"" escaped "\"\n"))
         "")
     (if (null? auth-mechs)
         ""
         (string-append "AuthMechs " (string-join auth-mechs " ") "\n"))
     "TLSType " tls-type "\n"
     "CertificateFile " cert-file "\n")))

(define (serialize-maildir-store config)
  (let ((name (home-mbsync-maildir-store-configuration-name config))
        (path (home-mbsync-maildir-store-configuration-path config))
        (inbox (home-mbsync-maildir-store-configuration-inbox config))
        (sub-folders (home-mbsync-maildir-store-configuration-sub-folders config))
        (flatten (home-mbsync-maildir-store-configuration-flatten config)))
    (string-append
     "MaildirStore " name "\n"
     "Path " path "\n"
     "Inbox " inbox "\n"
     (cond
      (flatten (string-append "Flatten " flatten "\n"))
      (sub-folders (string-append "SubFolders " sub-folders "\n"))
      (else "")))))

(define (serialize-channel config)
  (let ((name (home-mbsync-channel-configuration-name config))
        (far (home-mbsync-channel-configuration-far config))
        (near (home-mbsync-channel-configuration-near config))
        (patterns (home-mbsync-channel-configuration-patterns config))
        (create (home-mbsync-channel-configuration-create config))
        (expunge (home-mbsync-channel-configuration-expunge config))
        (sync-state (home-mbsync-channel-configuration-sync-state config)))
    (string-append
     "Channel " name "\n"
     "Far " far "\n"
     "Near " near "\n"
     (if (null? patterns)
         ""
         (string-append "Patterns " (string-join patterns " ") "\n"))
     "Create " create "\n"
     "Expunge " expunge "\n"
     "SyncState " sync-state "\n")))

(define (serialize-group config)
  (let ((name (home-mbsync-group-configuration-name config))
        (channels (home-mbsync-group-configuration-channels config)))
    (string-append
     "Group " name "\n"
     (string-join (map (lambda (ch)
                         (string-append "Channel " ch))
                       channels)
                  "\n")
     "\n")))

(define (serialize-account config)
  (let ((imap-store (home-mbsync-account-configuration-imap-store config))
        (maildir-store (home-mbsync-account-configuration-maildir-store config))
        (channels (home-mbsync-account-configuration-channels config))
        (group (home-mbsync-account-configuration-group config)))
    (string-append
     (serialize-imap-store imap-store)
     "\n"
     (serialize-maildir-store maildir-store)
     "\n"
     (string-join (map serialize-channel channels) "\n")
     (if group
         (string-append "\n" (serialize-group group))
         ""))))

(define (serialize-configuration config)
  (let ((accounts (home-mbsync-configuration-accounts config)))
    (string-join (map serialize-account accounts) "\n")))

;;;
;;; Service implementation
;;;

(define (add-mbsync-file config)
  `((".mbsyncrc"
     ,(plain-file "mbsyncrc" (serialize-configuration config)))))

(define (add-mbsync-packages config)
  (list (home-mbsync-configuration-package config)))

(define (add-mbsync-shepherd-service config)
  (let ((package (home-mbsync-configuration-package config))
        (interval (home-mbsync-configuration-sync-interval config)))
    (list
     (shepherd-service
      (provision '(mbsync))
      (documentation "Periodically synchronize mail with mbsync.")
      (start #~(make-forkexec-constructor
                (list #$(file-append package "/bin/mbsync") "--all")
                #:log-file (string-append
                            (or (getenv "XDG_STATE_HOME")
                                (string-append (getenv "HOME")
                                               "/.local/state"))
                            "/log/mbsync.log")))
      (stop #~(make-kill-destructor))
      (respawn? #t)
      (respawn-limit #~(cons 5 #$(* interval 5)))
      (respawn-delay interval)))))

(define (add-mbsync-activation config)
  (let ((paths (map (lambda (account)
                      (home-mbsync-maildir-store-configuration-path
                       (home-mbsync-account-configuration-maildir-store account)))
                    (home-mbsync-configuration-accounts config))))
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))
          (let ((home (getenv "HOME")))
            #$@(map (lambda (path)
                      #~(let ((resolved
                               (if (string-prefix? "~/" #$path)
                                   (string-append home "/" (substring #$path 2))
                                   #$path)))
                          (mkdir-p resolved)))
                    paths))))))

(define home-mbsync-service-type
  (service-type
   (name 'home-mbsync)
   (extensions
    (list
     (service-extension home-files-service-type add-mbsync-file)
     (service-extension home-shepherd-service-type add-mbsync-shepherd-service)
     (service-extension home-profile-service-type add-mbsync-packages)
     (service-extension home-activation-service-type add-mbsync-activation)))
   (description "Configure mbsync (isync) for IMAP mail synchronization.")))
