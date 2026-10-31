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

;;; Build, every night on this host, what another machine gets when it
;;; updates: the profile of `guix pull' for its channels, and its system and
;;; home configurations, taken from a git repository.  The guix publish of
;;; this host then serves them; its cache is warmed for every item, as it
;;; answers 404 for big items it has not baked yet.
;;;
;;; Each published run records the commits of the channels it pinned, so
;;; that the other machine pulls exactly those: see
;;; (johnlepikhin system services guix-prebuild run) for the layout.  The
;;; other machine should take only the commits from it, and keep the URLs
;;; and introductions of its own channels file.
;;;
;;; Secret placed by hand: DEPLOY-KEY-FILE, a read-only deploy key of the
;;; repository, owned by USER, mode 0400.

(define-module (johnlepikhin system services guix-prebuild)
  #:use-module (gnu packages base)                 ; coreutils
  #:use-module (gnu packages guile)                ; guile-json-4
  #:use-module (gnu packages ssh)                  ; openssh
  #:use-module (gnu packages version-control)      ; git
  #:use-module (gnu services)
  #:use-module (gnu services mcron)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (johnlepikhin system services monitoring)
  #:use-module (johnlepikhin system services utils)
  #:use-module (ice-9 match)
  #:export (guix-prebuild-target
            guix-prebuild-target?
            guix-prebuild-configuration
            guix-prebuild-configuration?
            %gitlab-known-hosts
            guix-prebuild-service-type))

;; A configuration to build, at a path of the repository.
(define-record-type* <guix-prebuild-target>
  guix-prebuild-target
  make-guix-prebuild-target
  guix-prebuild-target?
  (name       guix-prebuild-target-name)          ;"laptop-system"
  (kind       guix-prebuild-target-kind           ;'system or 'home
              (sanitize (lambda (kind)
                          (unless (memq kind '(system home))
                            (error "guix-prebuild-target: bad kind" kind))
                          kind)))
  (file       guix-prebuild-target-file)          ;relative to the repository
  ;; Directories of the repository for -L.
  (load-path  guix-prebuild-target-load-path
              (default '())))

;; Host keys of gitlab.com, as published on docs.gitlab.com.
(define %gitlab-known-hosts
  (plain-file
   "gitlab-known-hosts"
   "gitlab.com ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBFSMqzJeV9rUzU4kWitGjeR4PWSa29SPqJ1fVkhtj3Hw9xjLVXVYrU9QlYWrOLXBpQ6KWjbjTDTdDkoohFzgbEY=
gitlab.com ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCsj2bNKTBSpIYDEGk9KxsGh3mySTRgMtXL583qmBpzeQ+jqCMRgBqB98u3z++J1sKlXHWfM9dyhSevkMwSbhoR8XIq/U0tCNyokEi/ueaBMCvbcTHhO7FcwzY92WK4Yt0aGROY5qX2UKSeOvuP4D6TPqKF1onrSzH9bx9XUf2lEdWT/ia1NEKjunUqu1xOB/StKDHMoX4/OKyIzuS0q/T1zOATthvasJFoPrAjkohTyaDUz2LN5JoH839hViyEG82yB+MjcFV5MU3N1l1QL3cVUCh93xSaua1N85qivl+siMkPGbO5xR/En4iEY6K2XPASUEMaieWVNTRCtJ4S8H+9
gitlab.com ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAfuCHKVTjquxvt6CM6tdG4SLp1Btn/nOeHHE5UOzRdf
"))

(define-record-type* <guix-prebuild-configuration>
  guix-prebuild-configuration
  make-guix-prebuild-configuration
  guix-prebuild-configuration?
  ;; The account git and guix run as; it owns the state directory.  The
  ;; one the other machine uses, so that its home configuration sees the
  ;; same HOME.
  (user                guix-prebuild-configuration-user)
  (group               guix-prebuild-configuration-group
                       (default "users"))
  (repository          guix-prebuild-configuration-repository) ;git URL
  (branch              guix-prebuild-configuration-branch
                       (default "master"))
  (deploy-key-file     guix-prebuild-configuration-deploy-key-file
                       (default "/etc/guix-prebuild/deploy-key"))
  (known-hosts         guix-prebuild-configuration-known-hosts ;file-like
                       (default %gitlab-known-hosts))
  ;; The channels of the other machine, in the repository.
  (channels-file       guix-prebuild-configuration-channels-file
                       (default "channels.scm"))
  (targets             guix-prebuild-configuration-targets
                       (default '()))
  ;; Environment of the evaluation, as an alist; HOME is the user's.  It
  ;; matters to configurations that read variables, so it is fixed rather
  ;; than inherited from mcron.
  (environment         guix-prebuild-configuration-environment
                       (default '(("PATH" . "/run/current-system/profile/bin")
                                  ("LANG" . "C.UTF-8")
                                  ("GUIX_LOCPATH"
                                   . "/run/current-system/locale"))))
  (schedule            guix-prebuild-configuration-schedule
                       (default "0 2 * * *"))
  ;; Published runs kept, with their GC roots.
  (keep-generations    guix-prebuild-configuration-keep-generations
                       (default 2))
  ;; Seconds per step.
  (timeouts            guix-prebuild-configuration-timeouts
                       (default '(("fetch" . 300) ("describe" . 1800)
                                  ("build" . 28800) ("warm" . 7200))))
  (state-directory     guix-prebuild-configuration-state-directory
                       (default "/var/lib/guix-prebuild"))
  (log-directory       guix-prebuild-configuration-log-directory
                       (default "/var/log/guix-prebuild"))
  (publish-url         guix-prebuild-configuration-publish-url
                       (default "http://127.0.0.1:3000"))
  (publish-cache-directory
   guix-prebuild-configuration-publish-cache-directory
   (default "/var/cache/guix/publish"))
  (textfile-directory  guix-prebuild-configuration-textfile-directory
                       (default %node-textfile-directory)))

(define %gcroots-directory "/var/guix/gcroots/guix-prebuild")

(define %run-modules
  '((johnlepikhin system services guix-prebuild run)
    (johnlepikhin system services host-process)
    (johnlepikhin system services monitoring textfile)))

(define (target->list target)
  (match-record target <guix-prebuild-target> (name kind file load-path)
    (list name kind file load-path)))

(define (run-program config)
  (match-record config <guix-prebuild-configuration>
    (user repository branch deploy-key-file known-hosts channels-file targets
          environment keep-generations timeouts state-directory
          log-directory publish-url publish-cache-directory
          textfile-directory)
    (program-file
     "guix-prebuild-run"
     (with-extensions (list guile-json-4)
       (with-imported-modules %run-modules
         #~(begin
             (use-modules (johnlepikhin system services guix-prebuild run))
             ;; The log of the run, readable by anyone; root writes it.
             (let ((log (string-append
                         #$log-directory "/"
                         (strftime "%F" (localtime (current-time))) ".log")))
               (let ((port (open-file log "a")))
                 (redirect-port port (current-output-port))
                 (redirect-port port (current-error-port))))
             (run-guix-prebuild
              `((user . ,#$user)
                (repository . ,#$repository)
                (branch . ,#$branch)
                (deploy-key-file . ,#$deploy-key-file)
                (known-hosts . ,#$known-hosts)
                (channels-file . ,#$channels-file)
                (targets . ,'#$(map target->list targets))
                (environment . ,'#$environment)
                (keep . ,#$keep-generations)
                (timeouts . ,'#$timeouts)
                (state-directory . ,#$state-directory)
                (gcroots-directory . ,#$%gcroots-directory)
                (publish-url . ,#$publish-url)
                (publish-cache-directory . ,#$publish-cache-directory)
                (textfile-directory . ,#$textfile-directory)
                ;; The guix of the system only starts time-machine and
                ;; pull; the pinned channels decide the revision.
                (guix . "/run/current-system/profile/bin/guix")
                (git . ,#$(file-append git "/bin/git"))
                (ssh . ,#$(file-append openssh "/bin/ssh"))
                (timeout . ,#$(file-append coreutils "/bin/timeout"))
                (du . ,#$(file-append coreutils "/bin/du"))))))))))

(define (guix-prebuild-mcron-jobs config)
  (list #~(job #$(guix-prebuild-configuration-schedule config)
               #$(run-program config))))

(define (guix-prebuild-profile config)
  "The job as a command, to run it by hand: sudo guix-prebuild-run."
  (list (package
          (name "guix-prebuild-run")
          (version "0")
          (source #f)
          (build-system trivial-build-system)
          (arguments
           (list #:builder
                 #~(let ((bin (string-append #$output "/bin")))
                     (mkdir #$output)
                     (mkdir bin)
                     (symlink #$(run-program config)
                              (string-append bin "/guix-prebuild-run")))))
          (home-page "")
          (synopsis "Run the guix-prebuild job")
          (description "This runs the job of guix-prebuild-service-type.")
          (license license:gpl3+))))

(define (guix-prebuild-activation config)
  (match-record config <guix-prebuild-configuration>
    (user group deploy-key-file state-directory log-directory)
    #~(begin
        (use-modules (guix build utils))
        #$(data-directory-activation state-directory user group)
        (mkdir-p #$log-directory)
        (chmod #$log-directory #o755)
        (mkdir-p #$%gcroots-directory)
        #$(secret-file-check deploy-key-file user))))

(define guix-prebuild-service-type
  (service-type
   (name 'guix-prebuild)
   (extensions
    (list (service-extension mcron-service-type guix-prebuild-mcron-jobs)
          (service-extension profile-service-type guix-prebuild-profile)
          (service-extension activation-service-type
                             guix-prebuild-activation)))
   (description "Every night, build the guix pull profile and the system
and home configurations of another machine from a git repository, so that
the guix publish of this host serves them, and record the pinned channel
commits for that machine to pull.")))
