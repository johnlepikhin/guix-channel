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

(define-module (johnlepikhin home fdm)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages mail)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:export (home-fdm-macro-configuration
            home-fdm-macro-configuration?
            home-fdm-action-configuration
            home-fdm-action-configuration?
            home-fdm-account-configuration
            home-fdm-account-configuration?
            home-fdm-match-configuration
            home-fdm-match-configuration?
            home-fdm-configuration
            home-fdm-configuration?
            home-fdm-service-type))

;;;
;;; Documentation
;;;

;;; Module: (johnlepikhin home fdm)
;;;
;;; Guix Home service for fdm — a mail filtering and delivery agent.
;;; fdm fetches mail from various sources (maildir, IMAP, POP3, stdin)
;;; and filters it according to user-defined rules, delivering to
;;; maildirs, pipes, or other destinations.
;;;
;;; This module provides a hybrid approach to fdm configuration:
;;; structural elements (actions, accounts, matches) are typed records,
;;; while the content of conditions and action bodies remains as raw
;;; strings. This gives structural validation with full flexibility
;;; for any fdm directive.
;;;
;;; Record types:
;;;
;;;   <home-fdm-macro-configuration>
;;;     name  — string, macro name
;;;     value — string or number, macro value
;;;     type  — symbol: 'string (default) or 'numeric
;;;
;;;   <home-fdm-action-configuration>
;;;     name        — string, action name
;;;     sub-actions — list of strings, fdm action directives
;;;       Single sub-action generates a simple action.
;;;       Multiple sub-actions generate a compound action in { }.
;;;
;;;   <home-fdm-account-configuration>
;;;     name     — string, account name
;;;     type     — symbol: 'maildir, 'maildirs, 'stdin, 'imap,
;;;                'imaps, 'pop3, or 'pop3s
;;;     path     — string or #f, path/URL for the account
;;;     extra    — list of strings, additional options
;;;     disabled — boolean, whether account is disabled
;;;
;;;   <home-fdm-match-configuration>
;;;     condition — string, raw fdm match condition
;;;     actions   — list of strings, action names to invoke
;;;     continue  — boolean, whether to continue matching
;;;
;;;   <home-fdm-configuration>
;;;     package              — package, default: fdm
;;;     macros               — list of <home-fdm-macro-configuration>
;;;     settings             — list of strings, raw "set ..." lines
;;;     actions              — list of <home-fdm-action-configuration>
;;;     accounts             — list of <home-fdm-account-configuration>
;;;     matches              — list of <home-fdm-match-configuration>
;;;     sync-interval        — integer, seconds between runs (default 300)
;;;     activate-directories — list of strings, maildir paths to create
;;;     extra-content        — string or #f, raw config appended at end
;;;
;;; Shepherd service:
;;;
;;;   The service runs `fdm fetch` periodically using shepherd's
;;;   respawn mechanism. fdm fetches and filters mail, then exits.
;;;   Shepherd restarts it after sync-interval seconds.
;;;   Logs are written to ~/.local/state/log/fdm.log.
;;;
;;; Usage with mbsync:
;;;
;;;   A typical workflow: mbsync synchronizes mail from IMAP to local
;;;   maildirs, then fdm filters the incoming mail into folders.
;;;   Both services run independently via shepherd. Configure mbsync
;;;   to sync into a staging maildir, then use fdm to sort from there.
;;;
;;; Example configuration:
;;;
;;;   (service home-fdm-service-type
;;;     (home-fdm-configuration
;;;       (macros
;;;         (list
;;;           (home-fdm-macro-configuration
;;;             (name "mail-dir")
;;;             (value "/home/user/Mail"))))
;;;       (settings
;;;         (list "set maximum-size 50M"))
;;;       (actions
;;;         (list
;;;           (home-fdm-action-configuration
;;;             (name "inbox")
;;;             (sub-actions '("maildir \"${mail-dir}/INBOX\"")))
;;;           (home-fdm-action-configuration
;;;             (name "spam")
;;;             (sub-actions '("maildir \"${mail-dir}/Spam\"")))
;;;           (home-fdm-action-configuration
;;;             (name "drop")
;;;             (sub-actions '("drop")))
;;;           (home-fdm-action-configuration
;;;             (name "mark-and-spam")
;;;             (sub-actions
;;;               '("add-header \"X-Spam\" \"Yes\""
;;;                 "maildir \"${mail-dir}/Spam\"")))))
;;;       (accounts
;;;         (list
;;;           (home-fdm-account-configuration
;;;             (name "personal")
;;;             (type 'maildir)
;;;             (path "\"${mail-dir}/Incoming\"")
;;;             (extra '("new-only")))))
;;;       (matches
;;;         (list
;;;           (home-fdm-match-configuration
;;;             (condition "\"^X-Spam-Status: Yes\" in headers")
;;;             (actions '("spam")))
;;;           (home-fdm-match-configuration
;;;             (condition "\"^From:.*@example\\.com\" in headers")
;;;             (actions '("inbox")))
;;;           (home-fdm-match-configuration
;;;             (condition "all")
;;;             (actions '("inbox")))))
;;;       (activate-directories
;;;         '("/home/user/Mail/INBOX"
;;;           "/home/user/Mail/Spam"
;;;           "/home/user/Mail/Incoming"))))
;;;
;;; More examples:
;;;
;;;   ;; Filtering spam by headers:
;;;   (home-fdm-match-configuration
;;;     (condition "\"^X-Spam-Flag: YES\" in headers")
;;;     (actions '("spam")))
;;;
;;;   ;; Sorting by sender:
;;;   (home-fdm-match-configuration
;;;     (condition "\"^From:.*@github\\.com\" in headers")
;;;     (actions '("github")))
;;;
;;;   ;; Sorting by subject:
;;;   (home-fdm-match-configuration
;;;     (condition "\"^Subject:.*\\[PATCH\\]\" in headers")
;;;     (actions '("patches")))
;;;
;;;   ;; Compound action (add header + move):
;;;   (home-fdm-action-configuration
;;;     (name "tag-and-archive")
;;;     (sub-actions
;;;       '("add-header \"X-Tagged\" \"archive\""
;;;         "maildir \"${mail-dir}/Archive\"")))
;;;
;;;   ;; Using macros for paths:
;;;   (home-fdm-macro-configuration
;;;     (name "mail-dir")
;;;     (value "/home/user/Mail"))
;;;   (home-fdm-macro-configuration
;;;     (name "max-size")
;;;     (value "50")
;;;     (type 'numeric))
;;;
;;;   ;; Multiple accounts:
;;;   (home-fdm-account-configuration
;;;     (name "work")
;;;     (type 'maildirs)
;;;     (path "\"${mail-dir}/Work\""))
;;;   (home-fdm-account-configuration
;;;     (name "old-account")
;;;     (type 'maildir)
;;;     (path "\"${mail-dir}/OldInbox\"")
;;;     (disabled #t))
;;;
;;;   ;; Continue matching after a rule:
;;;   (home-fdm-match-configuration
;;;     (condition "\"^List-Id:.*guix\" in headers")
;;;     (actions '("tag-guix"))
;;;     (continue #t))

;;;
;;; Record types
;;;

(define-record-type* <home-fdm-macro-configuration>
  home-fdm-macro-configuration make-home-fdm-macro-configuration
  home-fdm-macro-configuration?
  (name  home-fdm-macro-configuration-name)
  (value home-fdm-macro-configuration-value)
  (type  home-fdm-macro-configuration-type
         (default 'string)))

(define-record-type* <home-fdm-action-configuration>
  home-fdm-action-configuration make-home-fdm-action-configuration
  home-fdm-action-configuration?
  (name        home-fdm-action-configuration-name)
  (sub-actions home-fdm-action-configuration-sub-actions))

(define-record-type* <home-fdm-account-configuration>
  home-fdm-account-configuration make-home-fdm-account-configuration
  home-fdm-account-configuration?
  (name     home-fdm-account-configuration-name)
  (type     home-fdm-account-configuration-type)
  (path     home-fdm-account-configuration-path
            (default #f))
  (extra    home-fdm-account-configuration-extra
            (default '()))
  (disabled home-fdm-account-configuration-disabled
            (default #f)))

(define-record-type* <home-fdm-match-configuration>
  home-fdm-match-configuration make-home-fdm-match-configuration
  home-fdm-match-configuration?
  (condition home-fdm-match-configuration-condition)
  (actions   home-fdm-match-configuration-actions)
  (continue  home-fdm-match-configuration-continue
             (default #f)))

(define-record-type* <home-fdm-configuration>
  home-fdm-configuration make-home-fdm-configuration
  home-fdm-configuration?
  (package              home-fdm-configuration-package
                        (default fdm))
  (macros               home-fdm-configuration-macros
                        (default '()))
  (settings             home-fdm-configuration-settings
                        (default '()))
  (actions              home-fdm-configuration-actions
                        (default '()))
  (accounts             home-fdm-configuration-accounts
                        (default '()))
  (matches              home-fdm-configuration-matches
                        (default '()))
  (sync-interval        home-fdm-configuration-sync-interval
                        (default 300))
  (activate-directories home-fdm-configuration-activate-directories
                        (default '()))
  (extra-content        home-fdm-configuration-extra-content
                        (default #f)))

;;;
;;; Serialization
;;;

(define (serialize-macro config)
  (let ((name (home-fdm-macro-configuration-name config))
        (value (home-fdm-macro-configuration-value config))
        (type (home-fdm-macro-configuration-type config)))
    (case type
      ((numeric)
       (string-append "%" name " = " (number->string value) "\n"))
      (else
       (string-append "$" name " = \"" value "\"\n")))))

(define (serialize-action config)
  (let ((name (home-fdm-action-configuration-name config))
        (sub-actions (home-fdm-action-configuration-sub-actions config)))
    (cond
     ((null? sub-actions)
      (error "fdm action must have at least one sub-action:" name))
     ((null? (cdr sub-actions))
      (string-append "action \"" name "\" " (car sub-actions) "\n"))
     (else
      (string-append "action \"" name "\" {\n"
                     (string-join
                      (map (lambda (sa)
                             (string-append "  " sa))
                           sub-actions)
                      "\n")
                     "\n}\n")))))

(define (serialize-account config)
  (let ((name (home-fdm-account-configuration-name config))
        (type (home-fdm-account-configuration-type config))
        (path (home-fdm-account-configuration-path config))
        (extra (home-fdm-account-configuration-extra config))
        (disabled (home-fdm-account-configuration-disabled config)))
    (string-append
     "account \"" name "\" "
     (if disabled "disabled " "")
     (symbol->string type)
     (if path (string-append " " path) "")
     (if (null? extra)
         ""
         (string-append " " (string-join extra " ")))
     "\n")))

(define (serialize-match config)
  (let ((condition (home-fdm-match-configuration-condition config))
        (actions (home-fdm-match-configuration-actions config))
        (continue (home-fdm-match-configuration-continue config)))
    (when (null? actions)
      (error "fdm match must have at least one action"))
    (string-append
     "match " condition
     " action"
     (if (pair? (cdr actions)) "s" "")
     " "
     (string-join (map (lambda (a) (string-append "\"" a "\"")) actions) " ")
     (if continue " continue" "")
     "\n")))

(define (serialize-configuration config)
  (let ((macros (home-fdm-configuration-macros config))
        (settings (home-fdm-configuration-settings config))
        (actions (home-fdm-configuration-actions config))
        (accounts (home-fdm-configuration-accounts config))
        (matches (home-fdm-configuration-matches config))
        (extra-content (home-fdm-configuration-extra-content config)))
    (string-append
     ;; Settings
     (if (null? settings)
         ""
         (string-append
          (string-join settings "\n")
          "\n\n"))
     ;; Macros
     (if (null? macros)
         ""
         (string-append
          (string-join (map serialize-macro macros) "")
          "\n"))
     ;; Actions
     (if (null? actions)
         ""
         (string-append
          (string-join (map serialize-action actions) "\n")
          "\n"))
     ;; Accounts
     (if (null? accounts)
         ""
         (string-append
          (string-join (map serialize-account accounts) "")
          "\n"))
     ;; Matches
     (if (null? matches)
         ""
         (string-join (map serialize-match matches) ""))
     ;; Extra content
     (if extra-content
         (string-append "\n" extra-content "\n")
         ""))))

;;;
;;; Service implementation
;;;

(define (add-fdm-packages config)
  (list (home-fdm-configuration-package config)))

(define (add-fdm-shepherd-service config)
  (let ((package (home-fdm-configuration-package config))
        (interval (home-fdm-configuration-sync-interval config)))
    (list
     (shepherd-service
      (provision '(fdm))
      (documentation "Periodically fetch and filter mail with fdm.")
      (start #~(make-forkexec-constructor
                (list #$(file-append package "/bin/fdm") "fetch")
                #:log-file (string-append
                            (or (getenv "XDG_STATE_HOME")
                                (string-append (getenv "HOME")
                                               "/.local/state"))
                            "/log/fdm.log")))
      (stop #~(make-kill-destructor))
      (respawn? #t)
      (respawn-limit #~(cons 5 #$(* interval 5)))
      (respawn-delay interval)))))

(define (add-fdm-activation config)
  (let ((dirs (home-fdm-configuration-activate-directories config))
        (conf-content (serialize-configuration config)))
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))
          (let* ((home (getenv "HOME"))
                 (conf-path (string-append home "/.fdm.conf")))
            ;; Write config with restricted permissions
            (call-with-output-file conf-path
              (lambda (port)
                (display #$conf-content port)))
            (chmod conf-path #o600)
            ;; Create maildir directories with correct permissions
            #$@(map (lambda (path)
                      #~(let ((resolved
                               (if (string-prefix? "~/" #$path)
                                   (string-append home "/" (substring #$path 2))
                                   #$path)))
                          (mkdir-p resolved)
                          (chmod resolved #o700)
                          (for-each (lambda (sub)
                                      (let ((sub-path (string-append resolved "/" sub)))
                                        (when (file-exists? sub-path)
                                          (chmod sub-path #o700))))
                                    '("cur" "new" "tmp"))))
                    dirs))))))

(define home-fdm-service-type
  (service-type
   (name 'home-fdm)
   (extensions
    (list
     (service-extension home-shepherd-service-type add-fdm-shepherd-service)
     (service-extension home-profile-service-type add-fdm-packages)
     (service-extension home-activation-service-type add-fdm-activation)))
   (description "Configure fdm for mail fetching and filtering.")))
