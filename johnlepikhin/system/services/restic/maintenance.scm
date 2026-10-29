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

;;; Server-side upkeep of restic repositories, run on the host from mcron.
;;; The clients only append; here, with a maintenance key of each
;;; repository, old snapshots are forgotten and pruned, the data is checked
;;; and the state is reported to the textfile directory of the node
;;; exporter.  Parsers are pure, for tests on recorded output.

(define-module (johnlepikhin system services restic maintenance)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports)
  #:use-module (json)
  #:use-module (johnlepikhin system services monitoring textfile)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-19)
  #:export (restic-time->seconds
            snapshot-summary
            newest-file-mtime
            freshness-metrics
            collect-restic-freshness
            run-restic-maintenance))


;;;
;;; Parsers.
;;;

(define (restic-time->seconds str)
  "Parse a restic snapshot time such as \"2026-10-29T10:00:01.5+06:00\"
into seconds since the epoch, or #f."
  (match (string-match
          "^([0-9]{4})-([0-9]{2})-([0-9]{2})T([0-9]{2}):([0-9]{2}):([0-9]{2})(\\.[0-9]+)?(Z|([+-])([0-9]{2}):([0-9]{2}))$"
          str)
    (#f #f)
    (m
     (let* ((n (lambda (i) (string->number (match:substring m i))))
            (offset (if (string=? (match:substring m 8) "Z")
                        0
                        (* (if (string=? (match:substring m 9) "-") -1 1)
                           (+ (* 3600 (n 10)) (* 60 (n 11))))))
            (date (make-date 0 (n 6) (n 5) (n 4) (n 3) (n 2) (n 1) offset)))
       (time-second (date->time-utc date))))))

(define* (snapshot-summary text now #:key (max-future 86400))
  "Summarize the JSON TEXT of `restic snapshots --json': the number of
snapshots, the time of the newest one not in the future, and how many lie
more than MAX-FUTURE seconds after NOW."
  (let* ((times (filter-map (lambda (snapshot)
                              (let ((time (assoc-ref snapshot "time")))
                                (and (string? time)
                                     (restic-time->seconds time))))
                            (vector->list (json-string->scm text))))
         (future (filter (lambda (t) (> t (+ now max-future))) times))
         (past (filter (lambda (t) (<= t (+ now max-future))) times)))
    `((count . ,(length times))
      (future . ,(length future))
      (newest . ,(if (null? past) #f (apply max past))))))


;;;
;;; Freshness, without keys.
;;;

(define (newest-file-mtime directory)
  "The newest mtime among the files of DIRECTORY, or #f."
  (let ((dir (false-if-exception (opendir directory))))
    (and dir
         (let loop ((newest #f))
           (match (readdir dir)
             ((? eof-object?)
              (closedir dir)
              newest)
             ((or "." "..") (loop newest))
             (name
              (let ((st (false-if-exception
                         (lstat (string-append directory "/" name)))))
                (loop (if (and st (eq? 'regular (stat:type st))
                               (or (not newest) (> (stat:mtime st) newest)))
                          (stat:mtime st)
                          newest)))))))))

(define (freshness-metrics entries)
  "Metrics from ENTRIES, a list of repository names and the newest mtime in
their snapshots/ directory (#f for none)."
  (filter-map (match-lambda
                ((name . mtime)
                 (and mtime
                      (metric "restic_repository_snapshot_mtime_seconds" mtime
                              `(("repository" . ,name))))))
              entries))

(define (collect-restic-freshness directory root repositories)
  "Report the newest snapshot file of each of REPOSITORIES, paths relative
to ROOT, to the textfile DIRECTORY."
  (run-collector
   "restic-freshness" directory
   (lambda ()
     (freshness-metrics
      (map (lambda (repository)
             (cons repository
                   (newest-file-mtime (string-append root "/" repository
                                                     "/snapshots"))))
           repositories)))
   #:max-age 900))


;;;
;;; Maintenance.
;;;

(define (run-as user program . args)
  "Run PROGRAM with ARGS as USER; return its exit status and output."
  (let* ((pw (getpwnam user))
         (pipe (pipe))
         (pid (primitive-fork)))
    (if (zero? pid)
        (catch #t
          (lambda ()
            (close-port (car pipe))
            (dup2 (fileno (cdr pipe)) 1)
            (setgroups #())
            (setgid (passwd:gid pw))
            (setuid (passwd:uid pw))
            (setenv "HOME" (passwd:dir pw))
            (apply execl program program args))
          (lambda _ (primitive-exit 127)))
        (begin
          (close-port (cdr pipe))
          (let ((output (get-string-all (car pipe))))
            (close-port (car pipe))
            (values (status:exit-val (cdr (waitpid pid))) output))))))

(define (log fmt . args)
  (apply format #t (string-append "~a " fmt "~%")
         (strftime "%F %T" (localtime (current-time))) args)
  (force-output))

(define (maintain-repository job restic timeout user root cache now)
  "Run the maintenance steps of JOB, an alist from the service, and return
its metrics.  Every step is attempted unless an earlier one makes it
unsafe; failures are reported, not raised."
  (let* ((name (assoc-ref job 'repository))
         (repo (string-append root "/" name))
         (labels `(("repository" . ,name)))
         (restic* (lambda (seconds . args)
                    (apply run-as user timeout (number->string seconds)
                           restic "--cache-dir" cache "--quiet" "-r" repo
                           "--password-file" (assoc-ref job 'key-file)
                           args)))
         (step (lambda (step-name ok?)
                 (log "~a: ~a ~a" name step-name (if ok? "ok" "FAILED"))
                 (metric "restic_maintenance_success" ok?
                         `(,@labels ("step" . ,step-name))))))
    (if (not (file-exists? (string-append repo "/config")))
        (begin
          (log "~a: no repository yet" name)
          (list (step "repository" #f)))
        (let*-values
            (((unlock-status unlock-output) (restic* 600 "unlock"))
             ((snapshots-status snapshots-output)
              (restic* 1800 "snapshots" "--json" "--retry-lock" "30m"))
             ((summary)
              (and (zero? snapshots-status)
                   (false-if-exception
                    (snapshot-summary snapshots-output now
                                      #:max-future
                                      (assoc-ref job 'max-future)))))
             ((sane?)
              (and summary
                   (zero? (assoc-ref summary 'future))
                   (<= (assoc-ref summary 'count)
                       (assoc-ref job 'max-snapshots)))))
          (when (and summary (not sane?))
            (log "~a: ~a snapshots, ~a in the future: forget skipped" name
                 (assoc-ref summary 'count) (assoc-ref summary 'future)))
          (let* ((forget-ok?
                  (and sane?
                       (zero? (apply restic* 3600 "forget" "--group-by" "host"
                                     "--retry-lock" "30m"
                                     (assoc-ref job 'keep-flags)))))
                 (prune-ok?
                  (and forget-ok?
                       (zero? (restic* (* 6 3600) "prune" "--retry-lock" "30m"
                                       "--max-repack-size"
                                       (assoc-ref job 'max-repack-size)))))
                 (check-ok?
                  (zero? (restic* (* 6 3600) "check" "--retry-lock" "30m"
                                  "--read-data-subset"
                                  (format #f "~a/30"
                                          (min 30 (date-day
                                                   (time-utc->date
                                                    (make-time time-utc 0
                                                               now))))))))
                 (size
                  (call-with-values
                      (lambda ()
                        (restic* 1800 "stats" "--json" "--mode" "raw-data"
                                 "--retry-lock" "30m"))
                    (lambda (status output)
                      (and (zero? status)
                           (assoc-ref (json-string->scm output)
                                      "total_size"))))))
            (filter
             identity
             (list (step "unlock" (zero? unlock-status))
                   (step "snapshots" (and summary #t))
                   (step "sanity" (and sane? #t))
                   (step "forget" forget-ok?)
                   (step "prune" prune-ok?)
                   (step "check" check-ok?)
                   (and summary
                        (metric "restic_repository_snapshots"
                                (assoc-ref summary 'count) labels))
                   (and summary
                        (metric "restic_repository_future_snapshots"
                                (assoc-ref summary 'future) labels))
                   (and summary (assoc-ref summary 'newest)
                        (metric (string-append "restic_repository_newest_"
                                               "snapshot_timestamp_seconds")
                                (assoc-ref summary 'newest) labels))
                   (and size
                        (metric "restic_repository_size_bytes" size
                                labels)))))))))

(define (run-restic-maintenance directory restic timeout user root cache jobs)
  "Maintain JOBS, alists, in the repositories under ROOT as USER with the
restic cache in CACHE, and write the results to
DIRECTORY/restic-maintenance.prom."
  (run-collector
   "restic-maintenance" directory
   (lambda ()
     (let ((now (current-time)))
       (append-map (lambda (job)
                     (maintain-repository job restic timeout user root cache
                                          now))
                   jobs)))
   ;; Daily; a missed day is reported by the backup rules.
   #:max-age (* 2 86400)))
