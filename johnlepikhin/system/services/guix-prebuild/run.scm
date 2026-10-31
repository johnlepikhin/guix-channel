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

;;; The job of `guix-prebuild-service-type': build what another machine will
;;; build when it updates, so that its guix publish serves it.  It runs as
;;; root; git and guix run as the configured user, and so does every write
;;; to the state directory, which that user owns.  Root only registers GC
;;; roots, after checking the paths it was given.
;;;
;;; State directory:
;;;   checkout/                  the configuration repository
;;;   generations/<n>/           one run: pinned.scm, pull-profile*,
;;;                              and, once published, commits.scm, info.scm
;;;   latest -> generations/<n>  the newest published run
;;;
;;; info.scm, version 1, is read by the updating machine:
;;;   (guix-prebuild (version 1) (generation N) (timestamp T)
;;;                  (configs-commit "…")
;;;                  (targets ((name "…") (out "/gnu/store/…")
;;;                            (drv "/gnu/store/….drv" or #f)) …))
;;; commits.scm is the list of (NAME "COMMIT") of the pinned channels.

(define-module (johnlepikhin system services guix-prebuild run)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports)
  #:use-module (johnlepikhin system services host-process)
  #:use-module (johnlepikhin system services monitoring textfile)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (web client)
  #:use-module (web response)
  #:use-module (web uri)
  #:export (channel-commits
            last-store-path
            store-path?
            store-path-hash
            publishable?
            stale-generations
            info->sexp
            prebuild-metrics
            run-guix-prebuild))


;;;
;;; Pure parts.
;;;

(define (commit? str)
  (and (string? str) (string-match "^[0-9a-f]{40}$" str) #t))

(define (channel-commits text)
  "The (NAME COMMIT) pairs of the channels in TEXT, the output of `guix
describe -f channels', read as data.  An error when a channel has no
commit."
  (define (field name fields)
    (any (match-lambda
           (((? (cut eq? <> name)) value) value)
           (_ #f))
         fields))
  (match (call-with-input-string text read)
    (('list channels ...)
     (map (match-lambda
            (('channel fields ...)
             (let ((name (match (field 'name fields)
                           (('quote (? symbol? name)) name)
                           (_ (error "channel without a name"))))
                   (commit (field 'commit fields)))
               (unless (commit? commit)
                 (error "channel without a commit" name))
               (list name commit))))
          channels))
    (_ (error "not a channel list"))))

(define %store-path-rx
  (make-regexp "^/gnu/store/[0-9a-df-np-sv-z]{32}-[^/]+$"))

(define (store-path? str)
  "Whether STR names an item directly under /gnu/store."
  (and (string? str) (regexp-exec %store-path-rx str) #t))

(define (store-path-hash path)
  (substring (basename path) 0 32))

(define (last-store-path text)
  "The last line of TEXT that is a store path, or #f."
  (find store-path?
        (reverse (map string-trim-both (string-split text #\newline)))))

(define (publishable? results)
  "Whether RESULTS, a list of (TARGET OUT ROOTED?), all built and rooted."
  (and (pair? results)
       (every (match-lambda
                ((_ out rooted?) (and out rooted? #t)))
              results)))

(define (stale-generations generations published current keep)
  "Of GENERATIONS, numbers, those to delete: all but the KEEP newest of
PUBLISHED and CURRENT."
  (let ((kept (cons current (take (sort published >)
                                  (min keep (length published))))))
    (filter (lambda (n) (not (memv n kept))) generations)))

(define (info->sexp generation timestamp configs-commit targets)
  "The info.scm of a run; TARGETS is a list of (NAME OUT DRV)."
  `(guix-prebuild
    (version 1)
    (generation ,generation)
    (timestamp ,timestamp)
    (configs-commit ,configs-commit)
    (targets ,(map (match-lambda
                     ((name out drv)
                      `((name ,name) (out ,out) (drv ,drv))))
                   targets))))

(define* (prebuild-metrics #:key steps targets durations warm-missing
                           last-success cache-bytes)
  "Metrics of a run: STEPS and TARGETS are alists of names to booleans,
DURATIONS of target names to seconds; the rest are numbers or #f."
  (append
   (map (match-lambda
          ((step . ok?)
           (metric "guix_prebuild_step_success" ok? `(("step" . ,step)))))
        steps)
   (map (match-lambda
          ((target . ok?)
           (metric "guix_prebuild_target_success" ok?
                   `(("target" . ,target)))))
        targets)
   (map (match-lambda
          ((target . seconds)
           (metric "guix_prebuild_duration_seconds" seconds
                   `(("target" . ,target)))))
        durations)
   (filter-map (match-lambda
                 ((name value) (and value (metric name value))))
               `(("guix_prebuild_warm_missing" ,warm-missing)
                 ("guix_prebuild_last_success_timestamp_seconds"
                  ,last-success)
                 ("guix_publish_cache_bytes" ,cache-bytes)))))


;;;
;;; Running.
;;;

(define (say fmt . args)
  (apply format #t (string-append "~a " fmt "~%")
         (strftime "%F %T" (localtime (current-time))) args)
  (force-output))

(define (config-ref config key)
  (match (assq key config)
    ((_ . value) value)
    (#f (error "missing configuration key" key))))

(define (delete-tree dir)
  (when (file-exists? dir)
    (nftw dir (lambda (file stat flag base level)
                (if (memq flag '(directory directory-processed))
                    (rmdir file)
                    (delete-file file))
                #t)
          'depth 'physical)))

(define (generation-numbers directory)
  (filter-map string->number (or (scandir directory) '())))

(define (run-guix-prebuild config)
  "Run the job described by CONFIG, an alist from the service."
  (let* ((user (config-ref config 'user))
         (state (config-ref config 'state-directory))
         (checkout (string-append state "/checkout"))
         (generations (string-append state "/generations"))
         (gcroots (config-ref config 'gcroots-directory))
         (timeout (config-ref config 'timeout))
         (timeouts (config-ref config 'timeouts))
         (guix (config-ref config 'guix))
         (git (config-ref config 'git))
         (environment (config-ref config 'environment))
         (git-environment
          (cons (cons "GIT_SSH_COMMAND"
                      (string-join
                       (list (config-ref config 'ssh)
                             "-i" (config-ref config 'deploy-key-file)
                             "-o" (string-append
                                   "UserKnownHostsFile="
                                   (config-ref config 'known-hosts))
                             "-o" "StrictHostKeyChecking=yes"
                             "-o" "BatchMode=yes")))
                environment))
         (now (current-time)))

    (define* (as-user step program args #:key (env environment))
      "Run PROGRAM with ARGS as the user under the timeout of STEP; return
its output on success, #f otherwise."
      (say "~a: ~a ~a" step program (string-join args))
      (let-values (((status output)
                    (run-as user timeout
                            (cons* "--kill-after=60"
                                   (number->string
                                    (or (assoc-ref timeouts step) 3600))
                                   program args)
                            #:environment env)))
        (if (zero? status)
            output
            (begin
              (say "~a: failed with status ~a" step status)
              #f))))

    (define (in-checkout path)
      (string-append checkout "/" path))

    (define (fetch)
      (and (if (file-exists? (in-checkout ".git"))
               (as-user "fetch" git
                        (list "-C" checkout "fetch" "--quiet" "origin"
                              (config-ref config 'branch))
                        #:env git-environment)
               (as-user "fetch" git
                        (list "clone" "--quiet" "--branch"
                              (config-ref config 'branch)
                              (config-ref config 'repository) checkout)
                        #:env git-environment))
           (as-user "fetch" git
                    (list "-C" checkout "reset" "--quiet" "--hard"
                          (string-append "origin/"
                                         (config-ref config 'branch))))
           (let ((head (as-user "fetch" git
                                (list "-C" checkout "rev-parse" "HEAD"))))
             (and head (string-trim-both head)))))

    (define (describe generation-dir)
      (let ((text (as-user "describe" guix
                           (list "time-machine" "-C"
                                 (in-checkout
                                  (config-ref config 'channels-file))
                                 "--" "describe" "-f" "channels"))))
        (and text
             (let ((commits (false-if-exception (channel-commits text)))
                   (pinned (string-append generation-dir "/pinned.scm")))
               (and commits
                    (call-as user
                             (lambda ()
                               (call-with-output-file pinned
                                 (lambda (port) (display text port)))))
                    (cons pinned commits))))))

    (define (build-target target pinned generation-dir)
      "Build TARGET, (NAME KIND FILE LOAD-PATH); return its output and
derivation, or #f."
      (match target
        ((name 'channels _ _)
         (let ((profile (string-append generation-dir "/pull-profile")))
           (and (as-user "build" guix
                         (list "pull" "-C" pinned "-p" profile))
                (let ((out (false-if-exception (canonicalize-path profile))))
                  (and (store-path? out) (list out #f))))))
        ((name kind file load-path)
         (let* ((command (symbol->string kind))
                (args (append (list "time-machine" "-C" pinned "--"
                                    command "build")
                              (append-map (lambda (dir)
                                            (list "-L" (in-checkout dir)))
                                          load-path)
                              (list (in-checkout file))))
                (out (let ((text (as-user "build" guix args)))
                       (and text (last-store-path text))))
                (drv (and out (eq? kind 'system)
                          (let ((text (as-user "build" guix
                                               (append args '("-d")))))
                            (and text (last-store-path text))))))
           (and out (list out drv))))))

    (define (add-root generation name out)
      "Register OUT, as root, after checking that it is a store item."
      (let ((link (string-append gcroots "/" (number->string generation)
                                 "-" name)))
        (and (store-path? out)
             (file-exists? out)
             (begin
               (when (false-if-exception (lstat link))
                 (delete-file link))
               (symlink out link)
               #t))))

    (define (warm paths)
      "Request the narinfo of every item in the closure of PATHS until all
are served or the warm budget is over; return how many are still missing."
      (let* ((text (as-user "warm" guix (cons* "gc" "-R" paths)))
             (items (if text
                        (filter store-path?
                                (string-split (string-trim-both text)
                                              #\newline))
                        '()))
             (url (config-ref config 'publish-url))
             (deadline (+ (current-time)
                          (or (assoc-ref timeouts "warm") 7200))))
        (define (served? item)
          (catch #t
            (lambda ()
              (let* ((uri (string->uri (string-append
                                        url "/" (store-path-hash item)
                                        ".narinfo")))
                     (socket (open-socket-for-uri uri)))
                (setsockopt socket SOL_SOCKET SO_RCVTIMEO '(30 . 0))
                (let-values (((response body)
                              (http-get uri #:port socket #:streaming? #t)))
                  (close-port socket)
                  (= 200 (response-code response)))))
            (const #f)))
        (if (not text)
            #f
            (let loop ((missing items) (pass 1))
              (let ((missing (remove served? missing)))
                (say "warm: pass ~a, ~a of ~a not served yet"
                     pass (length missing) (length items))
                (if (or (null? missing) (> (current-time) deadline))
                    (length missing)
                    (begin
                      (sleep 45)
                      (loop missing (+ pass 1)))))))))

    (define (publish generation generation-dir commits configs-commit
                     results)
      (call-as user
               (lambda ()
                 (call-with-output-file (string-append generation-dir
                                                       "/commits.scm")
                   (lambda (port) (write commits port) (newline port)))
                 (call-with-output-file (string-append generation-dir
                                                       "/info.scm")
                   (lambda (port)
                     (write (info->sexp generation now configs-commit
                                        results)
                            port)
                     (newline port)))
                 (let ((tmp (string-append state "/latest.tmp")))
                   (when (false-if-exception (lstat tmp))
                     (delete-file tmp))
                   (symlink (string-append "generations/"
                                           (number->string generation))
                            tmp)
                   (rename-file tmp (string-append state "/latest"))))))

    (define (published? n)
      (file-exists? (string-append generations "/" (number->string n)
                                   "/info.scm")))

    (define (cleanup current)
      (let* ((all (generation-numbers generations))
             (stale (stale-generations all (filter published? all) current
                                       (config-ref config 'keep))))
        (for-each
         (lambda (n)
           (let ((prefix (string-append (number->string n) "-")))
             (for-each (lambda (root)
                         (delete-file (string-append gcroots "/" root)))
                       (filter (cut string-prefix? prefix <>)
                               (or (scandir gcroots) '()))))
           (say "cleanup: generation ~a" n))
         stale)
        (call-as user
                 (lambda ()
                   (for-each (lambda (n)
                               (delete-tree (string-append
                                             generations "/"
                                             (number->string n))))
                             stale)))))

    (define (last-success)
      (let ((st (false-if-exception (lstat (string-append state "/latest")))))
        (and st (stat:mtime st))))

    (define (cache-bytes)
      (let ((dir (config-ref config 'publish-cache-directory)))
        (and (file-exists? dir)
             (let-values (((status output)
                           (command-output timeout 600
                                           (config-ref config 'du) "-sb"
                                           dir)))
               (and (zero? status)
                    (string->number (car (string-split output #\tab))))))))

    (run-collector
     "guix-prebuild" (config-ref config 'textfile-directory)
     (lambda ()
       (let* ((generation (+ 1 (fold max 0 (generation-numbers generations))))
              (generation-dir (string-append generations "/"
                                             (number->string generation)))
              (configs-commit (fetch))
              (made? (and configs-commit
                          (call-as user
                                   (lambda ()
                                     (unless (file-exists? generations)
                                       (mkdir generations))
                                     (mkdir generation-dir)))))
              (pinned (and made? (describe generation-dir)))
              (targets (cons '("channels" channels #f ())
                             (config-ref config 'targets)))
              (built
               (map (lambda (target)
                      (let* ((start (current-time))
                             (result (and pinned
                                          (build-target target (car pinned)
                                                        generation-dir)))
                             (rooted? (and result
                                           (add-root generation (car target)
                                                     (car result)))))
                        (say "~a: ~a" (car target)
                             (if result (car result) "FAILED"))
                        (list (car target) result rooted?
                              (- (current-time) start))))
                    targets))
              (results (map (match-lambda
                              ((name result rooted? _)
                               (list name (and result (car result)) rooted?)))
                            built))
              (ok? (publishable? results))
              (missing (and ok? (warm (map cadr results))))
              (published?
               (and ok?
                    (publish generation generation-dir (cdr pinned)
                             configs-commit
                             (map (match-lambda
                                    ((name (out drv) _ _) (list name out drv)))
                                  built))))
              (cleaned? (false-if-exception (cleanup generation))))
         (prebuild-metrics
          #:steps `(("fetch" . ,(and configs-commit #t))
                    ("describe" . ,(and pinned #t))
                    ("roots" . ,(every caddr built))
                    ("warm" . ,(eqv? missing 0))
                    ("publish" . ,(and published? #t))
                    ("cleanup" . ,(and cleaned? #t)))
          #:targets (map (match-lambda
                           ((name result _ _) (cons name (and result #t))))
                         built)
          #:durations (map (match-lambda
                             ((name _ _ seconds) (cons name seconds)))
                           built)
          #:warm-missing missing
          #:last-success (last-success)
          #:cache-bytes (cache-bytes))))
     ;; Daily, and a run may take hours.
     #:max-age (* 2 86400))))
