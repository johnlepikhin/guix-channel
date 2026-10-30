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

;;; Collectors for the textfile directory of the Prometheus node exporter.
;;; They run on the host from mcron, as root.  The parsers are pure and take
;;; the commands' output as strings, so they can be tested on recorded output.
;;;
;;; Every collector writes <name>.prom atomically and reports itself with
;;; textfile_collector_success and textfile_collector_last_run_timestamp_seconds
;;; under the label collector="<name>".  When a collection fails, the metrics
;;; of the previous run are kept, so that a broken collector does not look
;;; like a broken host; the success metric tells the two apart.

(define-module (johnlepikhin system services monitoring textfile)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:export (metric
            render-metrics
            herd-status-started
            shepherd-metrics
            smartctl-metrics
            image-reference
            newer-tag
            update-metrics
            run-collector
            collect-shepherd
            collect-smart
            collect-updates))


;;;
;;; Exposition format.
;;;

;; A sample: metric name, alist of label names to values, and a number.
(define* (metric name value #:optional (labels '()))
  (list name labels value))

(define (escape-label-value str)
  (string-concatenate
   (map (match-lambda
          (#\\ "\\\\")
          (#\" "\\\"")
          (#\newline "\\n")
          (chr (string chr)))
        (string->list str))))

(define (render-value value)
  (cond ((boolean? value) (if value "1" "0"))
        ((and (exact? value) (integer? value)) (number->string value))
        (else (number->string (exact->inexact value)))))

(define (render-sample sample)
  (match sample
    ((name labels value)
     (string-append
      name
      (if (null? labels)
          ""
          (string-append
           "{"
           (string-join
            (map (match-lambda
                   ((key . val)
                    (string-append key "=\"" (escape-label-value val) "\"")))
                 labels)
            ",")
           "}"))
      " " (render-value value) "\n"))))

(define (metric-type name)
  (if (string-suffix? "_total" name) "counter" "gauge"))

(define %metric-help
  '(("textfile_collector_success"
     . "Whether the last run of the textfile collector succeeded.")
    ("textfile_collector_last_run_timestamp_seconds"
     . "When the textfile collector last ran.")
    ("textfile_collector_max_age_seconds"
     . "How old the output of the collector may get.")
    ("restic_repository_snapshot_mtime_seconds"
     . "Newest file in the snapshots directory of the repository.")
    ("restic_maintenance_success"
     . "Whether the last run of a maintenance step succeeded.")
    ("restic_repository_snapshots" . "Snapshots in the repository.")
    ("restic_repository_future_snapshots"
     . "Snapshots dated beyond the allowed clock skew.")
    ("restic_repository_newest_snapshot_timestamp_seconds"
     . "Time of the newest snapshot that is not in the future.")
    ("restic_repository_size_bytes" . "Size of the repository data.")
    ("shepherd_service_running"
     . "Whether the shepherd service is running.")
    ("smart_device_info" . "Model and firmware of the drive.")
    ("smart_exit_status" . "Exit status of smartctl, a bit mask.")
    ("smart_passed" . "Whether the drive passes its SMART health check.")
    ("smart_critical_warning" . "NVMe critical warning bits.")
    ("smart_temperature_celsius" . "Composite temperature of the drive.")
    ("smart_warning_temperature_celsius"
     . "Warning composite temperature threshold of the drive.")
    ("smart_critical_temperature_celsius"
     . "Critical composite temperature threshold of the drive.")
    ("smart_available_spare_ratio" . "Remaining spare capacity.")
    ("smart_available_spare_threshold_ratio"
     . "Spare capacity below which the drive reports a critical warning.")
    ("smart_percentage_used"
     . "Estimate of the drive life used, in percent of its endurance.")
    ("smart_media_errors_total" . "Unrecovered data integrity errors.")
    ("smart_error_log_entries_total" . "Entries in the NVMe error log.")
    ("smart_unsafe_shutdowns_total" . "Shutdowns without prior notice.")
    ("smart_power_on_seconds" . "Time the drive has been powered on.")
    ("smart_data_units_written_total"
     . "Data written, in units of 512000 bytes.")
    ("container_image_update_available"
     . "Whether the registry has a newer release of the image.")
    ("container_image_check_success"
     . "Whether the tags of the image could be listed.")
    ("guix_system_generation_timestamp_seconds"
     . "When the current system generation was switched to.")))

(define (group-samples samples)
  "SAMPLES with those of each metric name together, in the order the names
first appear: the text format wants the lines of a metric contiguous."
  (let ((names (delete-duplicates (map car samples))))
    (append-map (lambda (name)
                  (filter (lambda (sample) (string=? (car sample) name))
                          samples))
                names)))

(define (render-metrics samples)
  "Render SAMPLES in the Prometheus text format, each metric name declared
once; names ending in _total are counters, the others gauges."
  (let loop ((samples (group-samples samples)) (declared '()) (out '()))
    (match samples
      (()
       (string-concatenate-reverse out))
      (((and sample (name . _)) . rest)
       (loop rest
             (if (member name declared) declared (cons name declared))
             (cons (render-sample sample)
                   (if (member name declared)
                       out
                       (cons* (string-append "# TYPE " name " "
                                             (metric-type name) "\n")
                              (string-append
                               "# HELP " name " "
                               (or (assoc-ref %metric-help name) name) "\n")
                              out))))))))


;;;
;;; Shepherd.
;;;

(define (herd-status-started text)
  "Return the names listed under \"Started:\" in the output of `herd status'."
  (let loop ((lines (string-split text #\newline))
             (in-started? #f)
             (started '()))
    (match lines
      (()
       (reverse started))
      ((line . rest)
       (cond ((string-null? line)
              (loop rest in-started? started))
             ((not (char-whitespace? (string-ref line 0)))
              ;; A section header such as "Started:" or "One-shot:".
              (loop rest (string=? line "Started:") started))
             ((and in-started? (string-match "^ \\+ (.+)$" line))
              => (lambda (m)
                   (loop rest #t (cons (match:substring m 1) started))))
             (else
              (loop rest in-started? started)))))))

(define (shepherd-metrics started watched)
  "Return shepherd_service_running for each of the WATCHED services."
  (map (lambda (service)
         (metric "shepherd_service_running"
                 (if (member service started) 1 0)
                 `(("service" . ,service))))
       watched))


;;;
;;; SMART.
;;;

(define (ref object . keys)
  "Follow KEYS through the nested JSON OBJECT; #f when one is missing."
  (fold (lambda (key value)
          (and value
               (cond ((and (string? key) (pair? value))
                      (assoc-ref value key))
                     (else #f))))
        object
        keys))

(define (smartctl-metrics device text exit-status)
  "Return the metrics of DEVICE from the JSON output TEXT of `smartctl --json
-a' and its EXIT-STATUS.  Raise an error when smartctl could not read the
device at all (bits 0 and 1 of the exit status)."
  (when (logtest exit-status #b11)
    (error "smartctl failed" device exit-status))
  (let* ((data (json-string->scm text))
         (name (basename device))
         (labels `(("device" . ,name)))
         (health (or (ref data "nvme_smart_health_information_log")
                     (error "no NVMe health log" device)))
         (samples
          (filter-map
           (match-lambda
             ((metric-name value)
              (and (number? value)
                   (metric metric-name value labels))))
           `(("smart_exit_status" ,exit-status)
             ("smart_passed" ,(match (ref data "smart_status" "passed")
                                (#t 1)
                                (#f 0)
                                (_ #f)))
             ("smart_critical_warning" ,(ref health "critical_warning"))
             ("smart_temperature_celsius" ,(ref data "temperature" "current"))
             ("smart_warning_temperature_celsius"
              ,(ref data "nvme_composite_temperature_threshold" "warning"))
             ("smart_critical_temperature_celsius"
              ,(ref data "nvme_composite_temperature_threshold" "critical"))
             ("smart_available_spare_ratio"
              ,(let ((spare (ref health "available_spare")))
                 (and spare (/ spare 100.))))
             ("smart_available_spare_threshold_ratio"
              ,(let ((threshold (ref health "available_spare_threshold")))
                 (and threshold (/ threshold 100.))))
             ("smart_percentage_used" ,(ref health "percentage_used"))
             ("smart_media_errors_total" ,(ref health "media_errors"))
             ("smart_error_log_entries_total"
              ,(ref health "num_err_log_entries"))
             ("smart_unsafe_shutdowns_total" ,(ref health "unsafe_shutdowns"))
             ("smart_power_on_seconds"
              ,(let ((hours (ref health "power_on_hours")))
                 (and hours (* 3600 hours))))
             ("smart_data_units_written_total"
              ,(ref health "data_units_written"))))))
    (cons (metric "smart_device_info" 1
                  `(,@labels
                    ("model" . ,(or (ref data "model_name") ""))
                    ("firmware" . ,(or (ref data "firmware_version") ""))))
          samples)))


;;;
;;; Updates.
;;;

(define (image-reference image)
  "Split IMAGE, as \"registry/name:tag@sha256:...\" or without the digest,
into the repository and the tag; #f when it has no tag."
  (let* ((name (match (string-index image #\@)
                 (#f image)
                 (at (substring image 0 at))))
         (colon (string-rindex name #\:))
         (slash (string-rindex name #\/)))
    (and colon (or (not slash) (> colon slash))
         (cons (substring name 0 colon) (substring name (+ colon 1))))))

(define (tag-version tag)
  "The prefix and the numbers of a release TAG such as \"v3.15.1\" or
\"2026.10.4\"; #f for anything else, such as \"v3.16.0-rc.0\" or
\"latest\"."
  (match (string-match "^([^0-9]*)([0-9]+(\\.[0-9]+)*)$" tag)
    (#f #f)
    (m (cons (match:substring m 1)
             (map string->number
                  (string-split (match:substring m 2) #\.))))))

(define (version>? a b)
  (match (list a b)
    ((() ()) #f)
    (((x . a*) (y . b*))
     (or (> x y) (and (= x y) (version>? a* b*))))))

(define (newer-tag current tags)
  "The newest of TAGS that is a release of the same form as the CURRENT tag
and newer than it, or #f."
  (match (tag-version current)
    (#f #f)
    ((prefix . version)
     (let loop ((tags tags) (best #f) (best-version version))
       (match tags
         (() best)
         ((tag . rest)
          (match (tag-version tag)
            (((? (lambda (p) (string=? p prefix))) . candidate)
             (if (and (= (length candidate) (length version))
                      (version>? candidate best-version))
                 (loop rest tag candidate)
                 (loop rest best best-version)))
            (_ (loop rest best best-version)))))))))

(define (update-metrics checks generation-time)
  "Metrics from CHECKS, a list of (IMAGE CURRENT-TAG . TAGS-OR-#F), #f when
the tags could not be listed, and from GENERATION-TIME, #f when unknown."
  (append
   (append-map
    (match-lambda
      ((image current . tags)
       (let ((labels `(("image" . ,image))))
         (cons (metric "container_image_check_success" (and tags #t) labels)
               (if tags
                   (let ((latest (newer-tag current tags)))
                     (list (metric "container_image_update_available"
                                   (and latest #t)
                                   `(,@labels
                                     ("current" . ,current)
                                     ("latest" . ,(or latest current))))))
                   '())))))
    checks)
   (if generation-time
       (list (metric "guix_system_generation_timestamp_seconds"
                     generation-time))
       '())))


;;;
;;; Running collectors.
;;;

(define (command-output timeout seconds program . args)
  "Run PROGRAM with ARGS under TIMEOUT, killed after SECONDS; return its exit
status and standard output."
  (let* ((port (apply open-pipe* OPEN_READ timeout "--kill-after=5"
                      (number->string seconds) program args))
         (output (get-string-all port))
         (status (close-pipe port)))
    (values (or (status:exit-val status) 255) output)))

(define (previous-samples file names)
  "Return the sample lines for metrics NAMES from FILE, if it exists."
  (if (file-exists? file)
      (filter (lambda (line)
                (any (lambda (name)
                       (or (string-prefix? (string-append name "{") line)
                           (string-prefix? (string-append name " ") line)
                           (string-prefix? (string-append "# TYPE " name " ")
                                           line)
                           (string-prefix? (string-append "# HELP " name " ")
                                           line)))
                     names))
              (string-split (call-with-input-file file get-string-all)
                            #\newline))
      '()))

(define* (run-collector name directory thunk
                        #:key (kept-metrics '()) (max-age 900))
  "Write the metrics returned by THUNK to DIRECTORY/NAME.prom.  If THUNK
raises an error, keep the lines of KEPT-METRICS from the previous file and
report the failure.  MAX-AGE, in seconds, is how old the file may get before
the collector counts as stalled.  Another run still holding the lock makes
this a no-op."
  (let ((lock (open-file (string-append directory "/." name ".lock") "a"))
        (file (string-append directory "/" name ".prom"))
        (tmp (string-append directory "/" name ".prom.tmp")))
    (when (catch 'system-error
            (lambda () (flock lock (logior LOCK_EX LOCK_NB)) #t)
            (const #f))
      (let* ((result
              (catch #t
                (lambda () (cons #t (thunk)))
                (lambda (key . args)
                  (format (current-error-port) "~a: ~a ~s~%" name key args)
                  (cons #f (previous-samples file kept-metrics)))))
             (success? (car result))
             (status
              (list (metric "textfile_collector_success" success?
                            `(("collector" . ,name)))
                    (metric "textfile_collector_last_run_timestamp_seconds"
                            (current-time)
                            `(("collector" . ,name)))
                    (metric "textfile_collector_max_age_seconds" max-age
                            `(("collector" . ,name))))))
        (call-with-output-file tmp
          (lambda (port)
            (display (if success?
                         (render-metrics (append (cdr result) status))
                         (string-append
                          (string-join (cdr result) "\n" 'suffix)
                          (render-metrics status)))
                     port)))
        (chmod tmp #o644)
        (rename-file tmp file)))
    (close-port lock)))

(define (collect-shepherd directory herd timeout watched)
  (run-collector
   "shepherd" directory
   (lambda ()
     (call-with-values (lambda () (command-output timeout 20 herd "status"))
       (lambda (status output)
         (let ((started (herd-status-started output)))
           ;; An empty list is what a failed or half-upgraded shepherd
           ;; gives; it must not read as every service being down.
           (unless (and (zero? status) (pair? started))
             (error "herd status failed" status))
           (shepherd-metrics started watched)))))
   #:kept-metrics '("shepherd_service_running")
   #:max-age 180))

(define (collect-smart directory smartctl timeout devices)
  (run-collector
   "smart" directory
   (lambda ()
     (append-map
      (lambda (device)
        (call-with-values
            (lambda ()
              (command-output timeout 60 smartctl "--json=c" "-a" device))
          (lambda (status output)
            (smartctl-metrics device output status))))
      devices))))

(define (collect-updates directory skopeo timeout images system-profile)
  "Report whether the registries have newer releases of IMAGES, references
with a tag, and when SYSTEM-PROFILE, the system profile link, last changed."
  (run-collector
   "updates" directory
   (lambda ()
     (update-metrics
      (filter-map
       (lambda (image)
         (match (image-reference image)
           (#f #f)
           ((repository . tag)
            (call-with-values
                (lambda ()
                  (command-output timeout 120 skopeo "list-tags"
                                  (string-append "docker://" repository)))
              (lambda (status output)
                (cons* repository tag
                       (and (zero? status)
                            (false-if-exception
                             (vector->list
                              (assoc-ref (json-string->scm output)
                                         "Tags"))))))))))
       images)
      (let ((st (false-if-exception (lstat system-profile))))
        (and st (stat:mtime st)))))
   ;; Daily.
   #:max-age (* 2 86400)))
