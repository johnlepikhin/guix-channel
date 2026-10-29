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

;;; Pieces for backups with `home-restic-backup-service-type' of Guix to a
;;; rest-server, see (johnlepikhin system services restic-rest-server):
;;;
;;; * `restic-wrapper' makes a package to put in the `restic' field of a
;;;   backup job.  It keeps the REST credentials out of the store, skips
;;;   backups while the server is out of reach and takes what to back up
;;;   from a published selection.
;;;
;;; * `home-backup-selection-service-type' publishes such a selection from
;;;   records.  A smarter selector can replace it later: the contract is in
;;;   (johnlepikhin home restic selection).

(define-module (johnlepikhin home restic)
  #:use-module (gnu home services)
  #:use-module (gnu home services mcron)
  #:use-module (gnu packages backup)               ; restic
  #:use-module (gnu packages gnupg)                ; guile-gcrypt
  #:use-module (gnu services)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:export (restic-wrapper
            restic-wrapper?
            restic-wrapper->package

            home-backup-selection-configuration
            home-backup-selection-configuration?
            home-backup-selection-service-type))

(define %selection-module '(johnlepikhin home restic selection))


;;;
;;; The wrapper.
;;;

(define-record-type* <restic-wrapper>
  restic-wrapper
  make-restic-wrapper
  restic-wrapper?
  (restic        restic-wrapper-restic
                 (default restic))
  ;; Absolute path of a file holding "user:password" for the rest-server;
  ;; #f for none.  Read at every run, never stored.
  (credentials   restic-wrapper-credentials
                 (default #f))
  ;; #f, or the host and port of the server: a backup that cannot reach
  ;; them is skipped instead of failing.
  (reachability  restic-wrapper-reachability
                 (default #f))
  ;; #f, or the absolute path of a published selection directory.
  (selection     restic-wrapper-selection
                 (default #f))
  ;; A selection older than this, in seconds, is refused.
  (max-age       restic-wrapper-max-age
                 (default (* 2 86400))))

(define (restic-wrapper-program wrapper)
  (match-record wrapper <restic-wrapper>
    (restic credentials reachability selection max-age)
    (program-file
     "restic"
     (with-extensions (list guile-gcrypt)
       (with-imported-modules (list %selection-module)
         #~(begin
             (use-modules #$%selection-module)
             (run-restic-wrapper #$(file-append restic "/bin/restic")
                                 #$credentials
                                 '#$reachability
                                 #$selection
                                 #$max-age
                                 (cdr (command-line)))))))))

(define (restic-wrapper->package wrapper)
  "A package whose bin/restic is WRAPPER, for the `restic' field of
`restic-backup-job'."
  (let ((restic (restic-wrapper-restic wrapper)))
    (package
      (name "restic-wrapper")
      (version (package-version restic))
      (source #f)
      (build-system trivial-build-system)
      (arguments
       (list #:modules '((guix build utils))
             #:builder
             #~(begin
                 (use-modules (guix build utils))
                 (let ((bin (string-append #$output "/bin")))
                   (mkdir-p bin)
                   (copy-file #$(restic-wrapper-program wrapper)
                              (string-append bin "/restic"))
                   (chmod (string-append bin "/restic") #o555)))))
      (home-page (package-home-page restic))
      (synopsis "restic with REST credentials and a backup selection")
      (description "This runs restic with the REST credentials from a file,
skips backups while the server is out of reach, and gives backups the paths
of a published selection.")
      (license license:gpl3+))))


;;;
;;; The selection.
;;;

(define-record-type* <home-backup-selection-configuration>
  home-backup-selection-configuration
  make-home-backup-selection-configuration
  home-backup-selection-configuration?
  ;; Absolute path where generations and "current" are published.
  (directory  home-backup-selection-configuration-directory)
  ;; Absolute paths to back up.  Keep this set stable and put the details
  ;; in EXCLUDE.
  (roots      home-backup-selection-configuration-roots)
  ;; restic exclude patterns.
  (exclude    home-backup-selection-configuration-exclude
              (default '()))
  ;; Republished this often, so that it never looks stale.
  (schedule   home-backup-selection-configuration-schedule
              (default "15 12 * * *")))

(define (selection-program config)
  (match-record config <home-backup-selection-configuration>
    (directory roots exclude)
    (program-file
     "publish-backup-selection"
     (with-extensions (list guile-gcrypt)
       (with-imported-modules (list %selection-module)
         #~(begin
             (use-modules #$%selection-module)
             (publish-selection #$directory '#$roots '#$exclude
                                "home-backup-selection" (current-time))))))))

(define (selection-activation config)
  #~(system* #$(selection-program config)))

(define (selection-mcron-jobs config)
  (list #~(job #$(home-backup-selection-configuration-schedule config)
               #$(selection-program config))))

(define home-backup-selection-service-type
  (service-type
   (name 'home-backup-selection)
   (extensions
    (list (service-extension home-activation-service-type
                             selection-activation)
          (service-extension home-mcron-service-type
                             selection-mcron-jobs)))
   (description "Publish what to back up, from fixed roots and exclude
patterns, as a selection for the restic wrapper.")))
