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

;;; GitLab Runner with the Docker executor.
;;;
;;; config.toml is serialized from records into the store with a placeholder
;;; in place of each runner token.  Tokens are secrets and must not reach the
;;; world-readable store, so every start of the service copies the template
;;; to the state directory and fills in the tokens from root-only files.  The
;;; copy is regenerated on each start: edits that `gitlab-runner' itself
;;; makes to it do not survive a restart.

(define-module (johnlepikhin system services gitlab-runner)
  #:use-module (gnu packages admin)                ; shadow
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (johnlepikhin packages gitlab-runner)
  #:export (gitlab-runner-docker-configuration
            gitlab-runner-docker-configuration?
            gitlab-runner-instance
            gitlab-runner-instance?
            gitlab-runner-configuration
            gitlab-runner-configuration?
            gitlab-runner-service-type))

(define-record-type* <gitlab-runner-docker-configuration>
  gitlab-runner-docker-configuration
  make-gitlab-runner-docker-configuration
  gitlab-runner-docker-configuration?
  ;; Used when a job does not name an image.
  (image        gitlab-runner-docker-configuration-image
                (default "alpine:latest"))
  (privileged?  gitlab-runner-docker-configuration-privileged?
                (default #f))
  (volumes      gitlab-runner-docker-configuration-volumes
                (default '("/cache")))
  (pull-policy  gitlab-runner-docker-configuration-pull-policy
                (default '("if-not-present"))))

(define-record-type* <gitlab-runner-instance>
  gitlab-runner-instance
  make-gitlab-runner-instance
  gitlab-runner-instance?
  (name        gitlab-runner-instance-name)           ;string
  (url         gitlab-runner-instance-url
               (default "https://gitlab.com"))
  ;; File holding the runner authentication token (glrt-...).  Read at
  ;; service start; keep it root-owned with mode 0600.
  (token-file  gitlab-runner-instance-token-file)     ;string
  ;; Maximum concurrent jobs of this runner; 0 means no limit beyond the
  ;; global `concurrent'.
  (limit       gitlab-runner-instance-limit
               (default 0))
  ;; Job requests in flight at once.  gitlab.com long-polls them, so with
  ;; upstream's default of 1 a burst of jobs starts one poll at a time.
  (request-concurrency gitlab-runner-instance-request-concurrency
                       (default 3))
  (docker      gitlab-runner-instance-docker
               (default (gitlab-runner-docker-configuration))))

(define-record-type* <gitlab-runner-configuration>
  gitlab-runner-configuration
  make-gitlab-runner-configuration
  gitlab-runner-configuration?
  (package          gitlab-runner-configuration-package
                    (default gitlab-runner))
  (user             gitlab-runner-configuration-user
                    (default "gitlab-runner"))
  (state-directory  gitlab-runner-configuration-state-directory
                    (default "/var/lib/gitlab-runner"))
  (concurrent       gitlab-runner-configuration-concurrent
                    (default 4))
  (check-interval   gitlab-runner-configuration-check-interval
                    (default 3))
  ;; Seconds to wait for running jobs when the service stops.  The runner is
  ;; sent SIGQUIT, on which it takes no new jobs and lets the running ones
  ;; finish; jobs still running after this are killed.
  (stop-timeout     gitlab-runner-configuration-stop-timeout
                    (default 600))
  (runners          gitlab-runner-configuration-runners  ;list of instances
                    (default '())))


;;;
;;; TOML serialization.
;;;

(define (toml-string str)
  (string-append
   "\""
   (string-concatenate
    (map (match-lambda
           (#\" "\\\"")
           (#\\ "\\\\")
           (#\newline "\\n")
           (chr (string chr)))
         (string->list str)))
   "\""))

(define (toml-value value)
  (match value
    ((? string?) (toml-string value))
    ((? boolean?) (if value "true" "false"))
    ((? integer?) (number->string value))
    ((? list?) (string-append "[" (string-join (map toml-value value) ", ")
                              "]"))))

(define (toml-pairs pairs)
  (string-concatenate
   (map (match-lambda
          ((key . value)
           (string-append key " = " (toml-value value) "\n")))
        pairs)))

(define (token-placeholder index)
  (string-append "@GITLAB_RUNNER_TOKEN_" (number->string index) "@"))

(define (serialize-runner runner index)
  (let ((docker (gitlab-runner-instance-docker runner)))
    (string-append
     "\n[[runners]]\n"
     (toml-pairs
      `(("name" . ,(gitlab-runner-instance-name runner))
        ("url" . ,(gitlab-runner-instance-url runner))
        ("token" . ,(token-placeholder index))
        ("executor" . "docker")
        ("limit" . ,(gitlab-runner-instance-limit runner))
        ("request_concurrency"
         . ,(gitlab-runner-instance-request-concurrency runner))))
     "\n[runners.docker]\n"
     (toml-pairs
      `(("image" . ,(gitlab-runner-docker-configuration-image docker))
        ("privileged"
         . ,(gitlab-runner-docker-configuration-privileged? docker))
        ("volumes" . ,(gitlab-runner-docker-configuration-volumes docker))
        ("pull_policy"
         . ,(gitlab-runner-docker-configuration-pull-policy docker)))))))

(define (gitlab-runner-config-template config)
  (let ((runners (gitlab-runner-configuration-runners config)))
    (plain-file
     "gitlab-runner-config.toml.in"
     (string-append
      (toml-pairs
       `(("concurrent" . ,(gitlab-runner-configuration-concurrent config))
         ("check_interval"
          . ,(gitlab-runner-configuration-check-interval config))))
      (string-concatenate
       (map serialize-runner runners (iota (length runners))))))))


;;;
;;; Service.
;;;

(define (gitlab-runner-config-file config)
  (string-append (gitlab-runner-configuration-state-directory config)
                 "/config.toml"))

;; Fill the tokens into the template and install the result, readable only
;; by the runner user, at its place in the state directory.
(define (gitlab-runner-config-program config)
  (let ((placeholders+files
         (map (lambda (runner index)
                (cons (token-placeholder index)
                      (gitlab-runner-instance-token-file runner)))
              (gitlab-runner-configuration-runners config)
              (iota (length (gitlab-runner-configuration-runners config))))))
    (program-file
     "gitlab-runner-config"
     #~(begin
         (use-modules (ice-9 string-fun)
                      (ice-9 textual-ports)
                      (srfi srfi-1))
         (let* ((target #$(gitlab-runner-config-file config))
                (temporary (string-append target ".new"))
                (user (getpwnam #$(gitlab-runner-configuration-user config)))
                (text (fold (lambda (placeholder+file text)
                              (let ((token
                                     (string-trim-both
                                      (call-with-input-file
                                          (cdr placeholder+file)
                                        get-string-all))))
                                (string-replace-substring
                                 text (car placeholder+file) token)))
                            (call-with-input-file
                                #$(gitlab-runner-config-template config)
                              get-string-all)
                            '#$placeholders+files)))
           (umask #o077)
           (call-with-output-file temporary
             (lambda (port) (put-string port text)))
           (chown temporary (passwd:uid user) (passwd:gid user))
           (rename-file temporary target))))))

(define (gitlab-runner-accounts config)
  (let ((user (gitlab-runner-configuration-user config)))
    (list (user-group (name user) (system? #t))
          (user-account
           (name user)
           (group user)
           ;; Access to the Docker socket; the group comes from
           ;; `docker-service-type'.
           (supplementary-groups '("docker"))
           (system? #t)
           (comment "GitLab Runner")
           (home-directory
            (gitlab-runner-configuration-state-directory config))
           (shell (file-append shadow "/sbin/nologin"))))))

(define (gitlab-runner-activation config)
  #~(begin
      (use-modules (guix build utils))
      (let ((directory #$(gitlab-runner-configuration-state-directory config))
            (user (getpwnam #$(gitlab-runner-configuration-user config))))
        (mkdir-p directory)
        (chown directory (passwd:uid user) (passwd:gid user))
        (chmod directory #o700))))

(define (gitlab-runner-shepherd-service config)
  (let ((user (gitlab-runner-configuration-user config))
        (directory (gitlab-runner-configuration-state-directory config)))
    (list (shepherd-service
           (provision '(gitlab-runner))
           (requirement '(user-processes networking dockerd))
           (documentation "Run GitLab CI/CD jobs.")
           (start
            #~(lambda args
                (and (zero? (system* #$(gitlab-runner-config-program config)))
                     (apply
                      (make-forkexec-constructor
                       (list #$(file-append
                                (gitlab-runner-configuration-package config)
                                "/bin/gitlab-runner")
                             "run"
                             "--config" #$(gitlab-runner-config-file config)
                             "--working-directory" #$directory)
                       #:user #$user
                       #:group #$user
                       #:supplementary-groups '("docker")
                       #:directory #$directory
                       #:environment-variables
                       (list (string-append "HOME=" #$directory))
                       #:log-file "/var/log/gitlab-runner.log")
                      args))))
           (stop #~(make-kill-destructor
                    SIGQUIT
                    #:grace-period
                    #$(gitlab-runner-configuration-stop-timeout config)))))))

(define gitlab-runner-service-type
  (service-type
   (name 'gitlab-runner)
   (extensions
    (list (service-extension account-service-type
                             gitlab-runner-accounts)
          (service-extension activation-service-type
                             gitlab-runner-activation)
          (service-extension shepherd-root-service-type
                             gitlab-runner-shepherd-service)
          (service-extension profile-service-type
                             (compose list
                                      gitlab-runner-configuration-package))))
   (description "Run GitLab Runner with the Docker executor.  Requires
@code{docker-service-type}.  Runner tokens are read from files at service start
and never enter the store.")))
