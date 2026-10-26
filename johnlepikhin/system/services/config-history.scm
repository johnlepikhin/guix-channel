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

;;; Change history of configuration that applications keep in their state
;;; directories, such as Home Assistant's automations and dashboards: an
;;; mcron job copies an allow-list of files into a git repository with the
;;; secrets replaced, commits when something changed and pushes.
;;;
;;; This is a history, not a backup: secrets, databases and runtime state
;;; stay out of it.  Pushing uses an SSH key that the activation generates on
;;; the host, outside the store; add its public half to the remote as a
;;; deploy key with write access.

(define-module (johnlepikhin system services config-history)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages version-control)
  #:use-module (gnu services)
  #:use-module (gnu services mcron)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:export (config-history-source
            config-history-source?

            %config-history-secret-keys

            config-history-configuration
            config-history-configuration?
            config-history-service-type))

(define-record-type* <config-history-source>
  config-history-source
  make-config-history-source
  config-history-source?
  (name      config-history-source-name)       ;subdirectory in the repository
  (directory config-history-source-directory)  ;string
  ;; Paths relative to DIRECTORY.  A "*" in the last component is a glob;
  ;; files that do not exist are skipped.
  (files     config-history-source-files))

;; Key names whose values are replaced, matched as described in
;; config-history-redact.py ("token" also covers "push_token").
(define %config-history-secret-keys
  '("password" "passwd" "secret" "token" "ssecurity" "cookie"
    "api_key" "private_key" "encryption_key" "network_key"
    "webhook_id" "cloudhook_url"))

(define-record-type* <config-history-configuration>
  config-history-configuration
  make-config-history-configuration
  config-history-configuration?
  (git          config-history-configuration-git
                (default git-minimal))
  (openssh      config-history-configuration-openssh
                (default openssh-sans-x))
  (python       config-history-configuration-python
                (default python-minimal))
  (directory    config-history-configuration-directory
                (default "/var/lib/config-history"))
  (branch       config-history-configuration-branch
                (default "master"))
  ;; SSH URL to push to, or #f to only commit locally.
  (remote       config-history-configuration-remote
                (default #f))
  ;; Lines for the known_hosts file of the remote.  When empty, the host key
  ;; is accepted on first use.
  (known-hosts  config-history-configuration-known-hosts
                (default '()))
  ;; Created by the activation if missing, readable by root only.
  (ssh-key      config-history-configuration-ssh-key
                (default "/etc/config-history/id_ed25519"))
  (author-name  config-history-configuration-author-name
                (default "config-history"))
  (author-email config-history-configuration-author-email
                (default "root@localhost"))
  (schedule     config-history-configuration-schedule
                (default "17 * * * *"))
  (secret-keys  config-history-configuration-secret-keys
                (default %config-history-secret-keys))
  (sources      config-history-configuration-sources)) ;list of sources

(define (known-hosts-file config)
  (string-append (dirname (config-history-configuration-ssh-key config))
                 "/known_hosts"))

(define (config-history-program config)
  (define git
    (file-append (config-history-configuration-git config) "/bin/git"))
  (define redact
    #~(list #$(file-append (config-history-configuration-python config)
                           "/bin/python3")
            #$(local-file "config-history-redact.py")
            #$(string-join (config-history-configuration-secret-keys config)
                           ",")))
  (define ssh-command
    (string-append "-i " (config-history-configuration-ssh-key config)
                   " -o IdentitiesOnly=yes -o BatchMode=yes"
                   " -o StrictHostKeyChecking="
                   (if (null? (config-history-configuration-known-hosts
                               config))
                       "accept-new"
                       "yes")
                   " -o UserKnownHostsFile=" (known-hosts-file config)))

  (program-file
   "config-history-snapshot"
   (with-imported-modules '((guix build utils))
     #~(begin
         (use-modules (guix build utils)
                      (ice-9 ftw)
                      (ice-9 match)
                      (ice-9 popen)
                      (ice-9 rdelim)
                      (ice-9 regex)
                      (srfi srfi-1))

         (define repository #$(config-history-configuration-directory config))
         (define branch #$(config-history-configuration-branch config))
         (define remote #$(config-history-configuration-remote config))

         (define (git . args)
           (apply invoke #$git "-C" repository args))

         (define (git-succeeds? . args)
           (zero? (apply system* #$git "-C" repository args)))

         (define (git-output . args)
           (let* ((port (apply open-pipe* OPEN_READ #$git "-C" repository
                               args))
                  (output (read-string port)))
             (close-pipe port)
             (string-trim-right output)))

         (define (glob->regexp glob)
           (make-regexp
            (string-append
             "^"
             (string-concatenate
              (map (match-lambda
                     (#\* ".*")
                     (char (regexp-quote (string char))))
                   (string->list glob)))
             "$")))

         (define (expand directory file)
           "Return the existing regular files that FILE names under
DIRECTORY, as paths relative to it."
           (define (regular? path)
             (and (file-exists? path)
                  (eq? 'regular (stat:type (stat path)))))
           (let ((base (basename file))
                 (parent (dirname file)))
             (if (string-index base #\*)
                 (let ((regexp (glob->regexp base))
                       (prefix (if (string=? parent ".")
                                   ""
                                   (string-append parent "/"))))
                   (filter-map
                    (lambda (name)
                      (and (regexp-exec regexp name)
                           (regular? (string-append directory "/" prefix
                                                    name))
                           (string-append prefix name)))
                    (or (scandir (string-append directory "/" parent))
                        '())))
                 (if (regular? (string-append directory "/" file))
                     (list file)
                     '()))))

         (setenv "GIT_AUTHOR_NAME"
                 #$(config-history-configuration-author-name config))
         (setenv "GIT_AUTHOR_EMAIL"
                 #$(config-history-configuration-author-email config))
         (setenv "GIT_COMMITTER_NAME" (getenv "GIT_AUTHOR_NAME"))
         (setenv "GIT_COMMITTER_EMAIL" (getenv "GIT_AUTHOR_EMAIL"))
         (setenv "GIT_SSH_COMMAND"
                 (string-append
                  #$(file-append (config-history-configuration-openssh config)
                                 "/bin/ssh")
                  " " #$ssh-command))

         (unless (file-exists? (string-append repository "/.git"))
           (mkdir-p repository)
           (chmod repository #o700)
           (git "init" "--quiet" "--initial-branch" branch))
         (when remote
           (if (member "origin" (string-split (git-output "remote")
                                              #\newline))
               (git "remote" "set-url" "origin" remote)
               (git "remote" "add" "origin" remote)))

         ;; Rebuild every source from scratch, so that files that are gone
         ;; or no longer listed leave the history too.
         (for-each
          (match-lambda
            ((name directory files)
             (let ((target (string-append repository "/" name)))
               (when (file-exists? target)
                 (delete-file-recursively target))
               (for-each
                (lambda (file)
                  (let ((output (string-append target "/" file)))
                    (mkdir-p (dirname output))
                    (apply invoke
                           (append #$redact
                                   (list (string-append directory "/" file)
                                         output)))))
                (delete-duplicates
                 (append-map (lambda (file) (expand directory file))
                             files))))))
          '#$(map (lambda (source)
                    (list (config-history-source-name source)
                          (config-history-source-directory source)
                          (config-history-source-files source)))
                  (config-history-configuration-sources config)))

         (git "add" "--all")
         (unless (git-succeeds? "diff" "--cached" "--quiet")
           (let ((changed (git-output "diff" "--cached" "--name-only")))
             (git "commit" "--quiet"
                  "-m" (string-append "Update "
                                      (string-join
                                       (delete-duplicates
                                        (map (lambda (file)
                                               (car (string-split file #\/)))
                                             (string-split changed
                                                          #\newline)))
                                       ", "))
                  "-m" changed)))

         ;; Push whenever the remote is behind, which also retries pushes
         ;; that failed earlier.
         (when (and remote
                    (not (string=? (git-output "rev-parse" "HEAD")
                                   (git-output "rev-parse" "--verify"
                                               "--quiet"
                                               (string-append
                                                "refs/remotes/origin/"
                                                branch)))))
           (git "push" "--quiet" "origin" branch))))))

(define (config-history-activation config)
  (let ((key (config-history-configuration-ssh-key config))
        (known-hosts (config-history-configuration-known-hosts config)))
    #~(begin
        (use-modules (guix build utils))
        (mkdir-p (dirname #$key))
        (chmod (dirname #$key) #o700)
        (unless (file-exists? #$key)
          (invoke #$(file-append (config-history-configuration-openssh config)
                                 "/bin/ssh-keygen")
                  "-q" "-t" "ed25519" "-N" "" "-C" "config-history"
                  "-f" #$key))
        (unless (null? '#$known-hosts)
          (call-with-output-file #$(known-hosts-file config)
            (lambda (port)
              (for-each (lambda (line) (display line port) (newline port))
                        '#$known-hosts)))))))

(define (config-history-mcron-jobs config)
  (list #~(job #$(config-history-configuration-schedule config)
               #$(config-history-program config))))

(define config-history-service-type
  (service-type
   (name 'config-history)
   (extensions
    (list (service-extension mcron-service-type config-history-mcron-jobs)
          (service-extension activation-service-type
                             config-history-activation)))
   (description "Keep the change history of selected configuration files in
a git repository, with their secrets redacted, and push it to a remote.")))
