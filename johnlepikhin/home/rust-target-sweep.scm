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

(define-module (johnlepikhin home rust-target-sweep)
  #:use-module (gnu home services)
  #:use-module (gnu home services mcron)
  #:use-module (gnu packages wm)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:export (home-rust-target-sweep-configuration
            home-rust-target-sweep-configuration?
            home-rust-target-sweep-configuration-roots
            home-rust-target-sweep-configuration-rules
            home-rust-target-sweep-configuration-incremental-age-days
            home-rust-target-sweep-configuration-dormant-age-days
            home-rust-target-sweep-configuration-exclude
            home-rust-target-sweep-configuration-prune-names
            home-rust-target-sweep-configuration-dry-run?
            home-rust-target-sweep-configuration-schedule
            home-rust-target-sweep-configuration-mcron?
            home-rust-target-sweep-configuration-disable-file
            home-rust-target-sweep-configuration-log-file
            home-rust-target-sweep-configuration-log-max-bytes
            home-rust-target-sweep-configuration-max-deletions
            home-rust-target-sweep-configuration-dormant-ratio-guard
            home-rust-target-sweep-configuration-incremental-ratio-guard
            home-rust-target-sweep-configuration-notify?
            %default-prune-names
            home-rust-target-sweep-service-type))

;;; Module: (johnlepikhin home rust-target-sweep)
;;;
;;; Weekly cleanup of Rust build artefacts in the user's home directory.
;;;
;;; rustc creates a fresh incremental directory per build configuration and
;;; never removes the old ones, so `target/*/incremental' grows without bound
;;; in long-lived projects; cargo's own garbage collector covers only
;;; ~/.cargo, never `target'.  Age is the only workable criterion: a directory
;;; untouched for a month belongs to a configuration that is no longer built,
;;; and deleting it costs one rebuild of that crate.
;;;
;;; The dangerous half of the job is deciding what "a Rust build directory"
;;; is.  A plain `find -name target' also matches a libvirt runtime directory,
;;; the contents of an npm package, and a Rust source module that happens to
;;; be called `target'.  This service therefore identifies build output
;;; positively, never by name alone -- see `classify-target' and `layout?' in
;;; the generated script, and the barriers documented there.
;;;
;;; Nothing outside a cargo *layout* (a per-configuration output directory,
;;; recognised by `.fingerprint' or `.cargo-lock' inside it) is ever removed.
;;; In particular the root of `target' survives, so `target/package/*.crate',
;;; `target/debian/*.deb' and `target/doc' are safe by construction rather
;;; than by a blacklist of names.
;;;
;;; The service ships with (dry-run? #t): the weekly job only writes a journal
;;; of what it would remove.  Flip it to #f once the journal looks right.
;;;
;;; Runbook:
;;;
;;;   * stop the weekly job without reconfiguring:
;;;       touch $XDG_STATE_HOME/rust-target-sweep.disable
;;;   * see what would be touched, machine-readable:
;;;       rust-target-sweep --print-targets
;;;   * journal of the last runs:
;;;       $XDG_STATE_HOME/log/rust-target-sweep.log
;;;   * deletion is irreversible and there are no filesystem snapshots; keep
;;;     the output of --print-targets and --dry-run as the only manifest of
;;;     what a run removed.
;;;
;;; Example:
;;;
;;;   (service home-rust-target-sweep-service-type
;;;     (home-rust-target-sweep-configuration
;;;       (dry-run? #f)
;;;       (exclude '("~/guix/infer"))))

(define %default-prune-names
  ;; Directory names never descended into.  Two reasons are mixed here: trees
  ;; that cannot contain a project of ours (`.git', `.cache'), and trees that
  ;; contain vendored copies of crates whose sources include directories
  ;; literally named `target' (`.cargo', `.cross-cargo', `node_modules').
  '(".git" ".cache" ".cargo" ".cross-cargo" ".rustup" ".guix-home"
    ".guix-profile" ".npm" ".venv" "node_modules" "Trash"))

(define-record-type* <home-rust-target-sweep-configuration>
  home-rust-target-sweep-configuration make-home-rust-target-sweep-configuration
  home-rust-target-sweep-configuration?
  ;; Directories to walk.  "~" and a leading "~/" are expanded at run time;
  ;; relative paths are refused.
  (roots                  home-rust-target-sweep-configuration-roots
                          (default '("~")))
  ;; Which rules to apply: 'incremental (stale incremental directories inside
  ;; live layouts) and 'dormant (layouts nothing has written to in a month).
  (rules                  home-rust-target-sweep-configuration-rules
                          (default '(incremental dormant)))
  (incremental-age-days   home-rust-target-sweep-configuration-incremental-age-days
                          (default 30))
  (dormant-age-days       home-rust-target-sweep-configuration-dormant-age-days
                          (default 30))
  ;; Absolute (or "~"-prefixed) paths never descended into, on top of
  ;; `prune-names'.
  (exclude                home-rust-target-sweep-configuration-exclude
                          (default '()))
  (prune-names            home-rust-target-sweep-configuration-prune-names
                          (default %default-prune-names))
  ;; #t: report only.  This is the default on purpose -- see the runbook.
  (dry-run?               home-rust-target-sweep-configuration-dry-run?
                          (default #t))
  (schedule               home-rust-target-sweep-configuration-schedule
                          (default "30 4 * * 0"))
  (mcron?                 home-rust-target-sweep-configuration-mcron?
                          (default #t))
  ;; #f -> $XDG_STATE_HOME/rust-target-sweep.disable
  (disable-file           home-rust-target-sweep-configuration-disable-file
                          (default #f))
  ;; #f -> $XDG_STATE_HOME/log/rust-target-sweep.log
  (log-file               home-rust-target-sweep-configuration-log-file
                          (default #f))
  ;; #f disables truncation.  The journal is the only record of what was
  ;; removed, so it is trimmed rather than rotated away.
  (log-max-bytes          home-rust-target-sweep-configuration-log-max-bytes
                          (default (* 4 1024 1024)))
  ;; A cap, not an abort: the excess is deferred to the next run.
  (max-deletions          home-rust-target-sweep-configuration-max-deletions
                          (default 50000))
  ;; Refuse to remove more than this fraction of all discovered layouts in one
  ;; run without --force.  #f disables the guard.
  (dormant-ratio-guard    home-rust-target-sweep-configuration-dormant-ratio-guard
                          (default 1/2))
  ;; Same idea for rule A, per target.  Off by default: a project simply not
  ;; built for a month legitimately has 100% stale incremental directories,
  ;; and a measured live tree already sits at 77%, so any threshold here
  ;; misfires on the healthy case.  Clock sanity (see the script) is what
  ;; actually guards against skew.
  (incremental-ratio-guard
   home-rust-target-sweep-configuration-incremental-ratio-guard
   (default #f))
  ;; Send a desktop notification with the run summary.
  (notify?                home-rust-target-sweep-configuration-notify?
                          (default #f)))


;;;
;;; The sweeper itself.
;;;

(define (sweep-script config)
  (let ((roots         (home-rust-target-sweep-configuration-roots config))
        (rules         (map symbol->string
                            (home-rust-target-sweep-configuration-rules config)))
        (inc-age       (home-rust-target-sweep-configuration-incremental-age-days config))
        (dormant-age   (home-rust-target-sweep-configuration-dormant-age-days config))
        (exclude       (home-rust-target-sweep-configuration-exclude config))
        (prune-names   (home-rust-target-sweep-configuration-prune-names config))
        (dry-run?      (home-rust-target-sweep-configuration-dry-run? config))
        (disable-file  (home-rust-target-sweep-configuration-disable-file config))
        (log-file      (home-rust-target-sweep-configuration-log-file config))
        (log-max-bytes (home-rust-target-sweep-configuration-log-max-bytes config))
        (max-deletions (home-rust-target-sweep-configuration-max-deletions config))
        (dormant-guard (home-rust-target-sweep-configuration-dormant-ratio-guard config))
        (inc-guard     (home-rust-target-sweep-configuration-incremental-ratio-guard config))
        (notify?       (home-rust-target-sweep-configuration-notify? config)))
    (program-file
     "rust-target-sweep"
     (with-imported-modules '((guix build utils))
       #~(begin
           (use-modules (guix build utils)
                        (ice-9 control)
                        (ice-9 ftw)
                        (ice-9 match)
                        (ice-9 rdelim)
                        (srfi srfi-1))

           ;;;
           ;;; Configuration baked in at build time.
           ;;;

           (define %conf-roots         (list #$@roots))
           (define %conf-rules         (list #$@rules))
           (define %conf-exclude       (list #$@exclude))
           (define %conf-prune-names   (list #$@prune-names))
           (define %conf-inc-age       #$inc-age)
           (define %conf-dormant-age   #$dormant-age)
           (define %conf-dry-run?      #$dry-run?)
           (define %conf-disable-file  #$disable-file)
           (define %conf-log-file      #$log-file)
           (define %conf-log-max-bytes #$log-max-bytes)
           (define %conf-max-deletions #$max-deletions)
           (define %conf-notify?       #$notify?)
           (define %dunstify           #$(if notify?
                                             (file-append dunst "/bin/dunstify")
                                             "/nonexistent"))

           ;; Ratios travel as strings: a gexp is a poor place for a rational
           ;; literal, and `string->number' reads "1/2" back exactly.
           (define %conf-dormant-guard
             #$(and dormant-guard (number->string dormant-guard)))
           (define %conf-inc-guard
             #$(and inc-guard (number->string inc-guard)))

           (define (guard-ratio spec)
             (and spec (string->number spec)))

           ;; The effective mode: the configured default, overridden by
           ;; --dry-run/--execute.  Everything that deletes consults this, not
           ;; the baked-in default.
           (define %dry-run? (make-parameter %conf-dry-run?))

           ;; Exit codes.  These are the only signal mcron sees.
           (define %exit-ok        0)
           (define %exit-errors    1)
           (define %exit-usage     2)
           (define %exit-guard     3)
           (define %exit-busy      4)

           ;; First line of the tag cargo drops into every target directory.
           (define %cachedir-signature
             "Signature: 8a477f597d28d172789f06886806bc55")

           (define %seconds-per-day 86400)


           ;;;
           ;;; Paths.
           ;;;

           (define %home
             (or (getenv "HOME")
                 (passwd:dir (getpwuid (getuid)))))

           (define (state-directory)
             (or (getenv "XDG_STATE_HOME")
                 (string-append %home "/.local/state")))

           (define (expand-user path)
             "Expand a leading \"~\" in PATH.  Relative paths are returned as
they are; callers reject them."
             (cond ((string=? path "~") %home)
                   ((string-prefix? "~/" path)
                    (string-append %home (substring path 1)))
                   (else path)))

           (define (path-components path)
             (remove string-null? (string-split path #\/)))

           (define (path-under? child parent)
             "True when CHILD is strictly below PARENT.  Compares path
components, not string prefixes: \"/a/bc\" is not below \"/a/b\"."
             (let ((c (path-components child))
                   (p (path-components parent)))
               (and (> (length c) (length p))
                    (equal? p (list-head c (length p))))))

           (define (path-sane? path)
             "Reject anything that must never reach a deletion primitive."
             (let ((parts (path-components path)))
               (and (string-prefix? "/" path)
                    (pair? parts)
                    (not (member ".." parts)))))

           (define (safe-lstat path)
             (catch 'system-error (lambda () (lstat path)) (const #f)))

           (define (safe-canonicalize path)
             (catch #t (lambda () (canonicalize-path path)) (const #f)))

           (define (directory-not-symlink? path)
             (let ((st (safe-lstat path)))
               (and st (eq? 'directory (stat:type st)))))

           (define (regular-file? path)
             (let ((st (safe-lstat path)))
               (and st (eq? 'regular (stat:type st)))))

           (define (directory-entries path)
             "Names in PATH, without \".\" and \"..\".  Unreadable directory
yields the empty list."
             (catch 'system-error
               (lambda ()
                 (let ((port (opendir path)))
                   (let loop ((acc '()))
                     (let ((entry (readdir port)))
                       (cond ((eof-object? entry)
                              (closedir port)
                              (sort acc string<?))
                             ((or (string=? entry ".") (string=? entry ".."))
                              (loop acc))
                             (else (loop (cons entry acc))))))))
               (const '())))


           ;;;
           ;;; Journal.
           ;;;

           (define %log-port (make-parameter (current-error-port)))
           (define %counters (make-hash-table))

           (define (bump! key . amount)
             (hash-set! %counters key
                        (+ (hash-ref %counters key 0)
                           (if (null? amount) 1 (car amount)))))

           (define (counter key) (hash-ref %counters key 0))

           (define (timestamp)
             (strftime "%Y-%m-%dT%H:%M:%S%z" (localtime (current-time))))

           (define (log! level rule reason bytes path)
             (format (%log-port) "~a ~a ~a ~a ~a ~a~%"
                     (timestamp) level rule reason bytes path)
             (force-output (%log-port)))

           (define (note! text)
             (format (%log-port) "~a ~a~%" (timestamp) text)
             (force-output (%log-port)))

           (define (log-file-path)
             (expand-user
              (or %conf-log-file
                  (string-append (state-directory)
                                 "/log/rust-target-sweep.log"))))

           (define (truncate-log! path)
             "Trim PATH to half of `log-max-bytes', cutting at a line
boundary.  Runs once at start-up, under the global lock, through a temporary
file so a reader never sees a half-written journal."
             (let ((st (and %conf-log-max-bytes (safe-lstat path))))
               (when (and st (> (stat:size st) %conf-log-max-bytes))
                 (let* ((keep (quotient %conf-log-max-bytes 2))
                        (tmp  (string-append path ".tmp")))
                   (catch #t
                     (lambda ()
                       (call-with-input-file path
                         (lambda (in)
                           (seek in (- (stat:size st) keep) SEEK_SET)
                           (read-line in)     ;drop the partial first line
                           (call-with-output-file tmp
                             (lambda (out)
                               (format out "~a --- журнал усечён ---~%"
                                       (timestamp))
                               (let loop ()
                                 (let ((line (read-line in 'concat)))
                                   (unless (eof-object? line)
                                     (display line out)
                                     (loop))))))))
                       (chmod tmp #o600)
                       (rename-file tmp path))
                     (lambda args
                       (false-if-exception (delete-file tmp))))))))


           ;;;
           ;;; Locking.
           ;;;

           (define (try-lock path)
             "Open PATH, creating it if needed, and take a non-blocking
exclusive flock.  Return the port on success, #f when somebody else holds it.
The caller must keep the returned port alive: in Guile the lock belongs to the
port and is dropped when the port is closed or collected."
             (catch 'system-error
               (lambda ()
                 (let ((port (open-file path "a")))
                   (catch 'system-error
                     (lambda ()
                       (flock port (logior LOCK_EX LOCK_NB))
                       port)
                     (lambda args
                       (close-port port)
                       #f))))
               (const #f)))


           ;;;
           ;;; Classification.  These are the barriers; everything below
           ;;; them assumes they have run.
           ;;;

           (define (cachedir-tag? directory)
             "True when DIRECTORY carries the cache-directory tag cargo
writes.  The signature line is checked, not merely the file name."
             (let ((tag (string-append directory "/CACHEDIR.TAG")))
               (and (regular-file? tag)
                    (catch #t
                      (lambda ()
                        (call-with-input-file tag
                          (lambda (port)
                            (equal? (read-line port) %cachedir-signature))))
                      (const #f)))))

           (define (layout? directory)
             "True when DIRECTORY is a cargo output layout -- the build
directory of one configuration.  Positive test: cargo puts `.fingerprint' and
`.cargo-lock' there and nowhere else.  This is what separates `debug',
`release', `deploy', `fastdev' from `package', `debian', `doc', `tmp'."
             (and (directory-not-symlink? directory)
                  (or (directory-not-symlink?
                       (string-append directory "/.fingerprint"))
                      (regular-file?
                       (string-append directory "/.cargo-lock")))))

           (define (target-layouts target)
             "Layouts of TARGET, both plain (target/debug) and cross-compiled
(target/<triple>/release)."
             (append-map
              (lambda (name)
                (let ((child (string-append target "/" name)))
                  (cond ((layout? child) (list child))
                        ((directory-not-symlink? child)
                         (filter-map
                          (lambda (grandchild)
                            (let ((path (string-append child "/" grandchild)))
                              (and (layout? path) path)))
                          (directory-entries child)))
                        (else '()))))
              (directory-entries target)))

           (define (classify-target directory)
             "Decide whether DIRECTORY is a cargo target directory.  Returns
(accept . fingerprint) or (reject . reason)."
             (let ((st (safe-lstat directory)))
               (cond
                ((not st)
                 (cons 'reject "barrier-2.2: исчез во время обхода"))
                ((not (eq? 'directory (stat:type st)))
                 (cons 'reject "barrier-2.2: не каталог или симлинк"))
                ((not (= (stat:uid st) (getuid)))
                 (cons 'reject "barrier-2.3: чужой владелец"))
                ((not (regular-file?
                       (string-append (dirname directory) "/Cargo.toml")))
                 (cons 'reject "barrier-2.4: рядом нет Cargo.toml"))
                ((cachedir-tag? directory)
                 (cons 'accept "CACHEDIR.TAG"))
                ((regular-file? (string-append directory "/.rustc_info.json"))
                 (cons 'accept ".rustc_info.json"))
                ((pair? (target-layouts directory))
                 (cons 'accept "layout"))
                (else
                 (cons 'reject "barrier-2.5: нет отпечатков cargo")))))


           ;;;
           ;;; Discovery.
           ;;;

           (define (discover-targets root prune-names excludes report)
             "Walk ROOT and return its cargo target directories.  REPORT is
called with (path verdict) for every directory named `target', accepted or
not.  A recognised target is never descended into."
             (let ((canonical (safe-canonicalize root)))
               (if (not canonical)
                   '()
                   (let* ((root-stat (safe-lstat canonical))
                          (device    (and root-stat (stat:dev root-stat)))
                          (found     '()))
                     (when device
                       (file-system-fold
                        (lambda (path st result) ;enter?
                          (cond
                           ((string=? path canonical) #t)
                           ((not (= device (stat:dev st))) #f)
                           ((member (basename path) prune-names) #f)
                           ((member path excludes) #f)
                           ((string=? (basename path) "target")
                            (let ((verdict (classify-target path)))
                              (report path verdict)
                              (when (eq? 'accept (car verdict))
                                (set! found (cons path found)))
                              ;; Either way: a target is a leaf of this walk.
                              #f))
                           (else #t)))
                        (lambda (path st result)         ;leaf
                          ;; `enter?' only ever sees directories, so a
                          ;; symlinked `target' would otherwise pass without
                          ;; leaving a trace.  It is never followed and never
                          ;; deleted; report it so the manifest is complete.
                          (when (and (eq? 'symlink (stat:type st))
                                     (string=? (basename path) "target"))
                            (report path
                                    (cons 'reject "barrier-2.2: симлинк")))
                          result)
                        (lambda (path st result) result)  ;down
                        (lambda (path st result) result)  ;up
                        (lambda (path st result) result)  ;skip
                        (lambda (path st errno result) result)
                        #t
                        canonical
                        lstat))
                     (reverse found)))))


           ;;;
           ;;; Sizes and freshness.
           ;;;

           (define (accumulate-blocks st seen)
             "Bytes ST occupies, in the same units `du' uses.  Hard links are
counted once."
             (let ((bytes (* 512 (stat:blocks st))))
               (if (> (stat:nlink st) 1)
                   (let ((key (cons (stat:dev st) (stat:ino st))))
                     (if (hash-ref seen key)
                         0
                         (begin (hash-set! seen key #t) bytes)))
                   bytes)))

           (define (tree-size path)
             "Disk usage of PATH, gathered in one walk."
             (let ((seen  (make-hash-table))
                   (total 0))
               (file-system-fold
                (lambda (p st result) #t)
                (lambda (p st result)
                  (set! total (+ total (accumulate-blocks st seen)))
                  result)
                (lambda (p st result)
                  (set! total (+ total (accumulate-blocks st seen)))
                  result)
                (lambda (p st result) result)
                (lambda (p st result) result)
                (lambda (p st errno result) result)
                #t path lstat)
               total))

           (define (contains-file-newer-than? path cutoff)
             "True when any regular file below PATH has mtime above CUTOFF.
Stops at the first one."
             (call/ec
              (lambda (return)
                (file-system-fold
                 (lambda (p st result) #t)
                 (lambda (p st result)
                   (if (> (stat:mtime st) cutoff) (return #t) result))
                 (lambda (p st result) result)
                 (lambda (p st result) result)
                 (lambda (p st result) result)
                 (lambda (p st errno result) result)
                 #f path lstat)
                #f)))

           (define (layout-activity-mtime target layout)
             "Cheap freshness proxy for LAYOUT: the newest of the two files
cargo actually rewrites on every build.  `.cargo-lock' deliberately does not
take part -- cargo only flocks it, so its mtime can be months old in a tree
built today."
             (fold max 0
                   (filter-map (lambda (path)
                                 (let ((st (safe-lstat path)))
                                   (and st (stat:mtime st))))
                               (list (string-append target "/.rustc_info.json")
                                     (string-append layout "/.fingerprint")))))


           ;;;
           ;;; Deletion.
           ;;;

           (define (remove-tree! path)
             "Delete PATH recursively and report whether it is really gone.
`delete-file-recursively' reports but ignores errors, so the check is ours to
make; its warnings are redirected into the journal."
             (parameterize ((current-error-port (%log-port)))
               (catch #t
                 (lambda () (delete-file-recursively path))
                 (lambda args
                   (log! "ERROR" "-" (format #f "~s" args) 0 path))))
             (not (safe-lstat path)))

           (define (delete-candidate! rule path bytes)
             "Barrier 7: delete PATH, keeping the counters honest."
             (if (%dry-run?)
                 (begin (log! "DRY" rule "-" bytes path)
                        (bump! 'would-delete)
                        (bump! 'would-free bytes)
                        #t)
                 (if (remove-tree! path)
                     (begin (log! "DELETE" rule "-" bytes path)
                            (bump! 'deleted)
                            (bump! 'freed bytes)
                            #t)
                     (begin (log! "FAIL" rule "не удалён полностью" bytes path)
                            (bump! 'failed)
                            #f))))


           ;;;
           ;;; Rule A -- stale incremental directories.
           ;;;

           (define (incremental-candidates target layout cutoff)
             "Direct children of LAYOUT/incremental older than CUTOFF, as
(path mtime bytes).  Barrier 3 for rule A is applied here: the path must sit
strictly inside the layout."
             (let ((incremental (string-append layout "/incremental")))
               (if (not (directory-not-symlink? incremental))
                   '()
                   (let* ((names (remove (lambda (n) (string-prefix? "." n))
                                         (directory-entries incremental)))
                          (total (length names))
                          (stale
                           (filter-map
                            (lambda (name)
                              (let* ((path (string-append incremental "/" name))
                                     (st   (safe-lstat path)))
                                (and st
                                     (eq? 'directory (stat:type st))
                                     (< (stat:mtime st) cutoff)
                                     (path-sane? path)
                                     (path-under? path layout)
                                     (list path (stat:mtime st)))))
                            names)))
                     (if (and (guard-ratio %conf-inc-guard)
                              (> total 0)
                              (> (/ (length stale) total)
                                 (guard-ratio %conf-inc-guard)))
                         (begin
                           (log! "REJECT" "A" "barrier-6: доля протухших выше порога"
                                 0 incremental)
                           (bump! 'guard-skipped)
                           '())
                         stale)))))

           (define (run-rule-a targets cutoff budget)
             "Apply rule A across TARGETS.  Candidates are collected first and
deleted oldest-first so that a run capped by `max-deletions' always makes
progress on the most stale entries.  Returns the remaining budget."
             (let* ((candidates
                     (append-map
                      (lambda (target)
                        (append-map
                         (lambda (layout)
                           (map (lambda (entry)
                                  (cons layout entry))
                                (incremental-candidates target layout cutoff)))
                         (target-layouts target)))
                      targets))
                    ;; (layout path mtime); oldest first.
                    (ordered (sort candidates
                                   (lambda (a b)
                                     (< (third a) (third b)))))
                    (taken   (if (> (length ordered) budget)
                                 (list-head ordered budget)
                                 ordered))
                    (deferred (- (length ordered) (length taken))))
               (bump! 'candidates-a (length ordered))
               (when (> deferred 0)
                 (bump! 'deferred deferred))
               ;; Group by layout so each layout is locked exactly once.
               (let loop ((rest taken) (left budget))
                 (if (null? rest)
                     left
                     (let* ((layout (first (car rest)))
                            (group  (filter (lambda (e)
                                              (string=? (first e) layout))
                                            rest))
                            (others (remove (lambda (e)
                                              (string=? (first e) layout))
                                            rest))
                            (lock   (try-lock (string-append layout
                                                             "/.cargo-lock"))))
                       (if (not lock)
                           (begin
                             (log! "SKIP-LOCKED" "A" "идёт сборка" 0 layout)
                             (bump! 'skipped-locked (length group))
                             (loop others left))
                           (let ((used
                                  (fold (lambda (entry used)
                                          (let* ((path  (second entry))
                                                 (bytes (tree-size path)))
                                            (if (delete-candidate! "A" path bytes)
                                                (+ used 1)
                                                used)))
                                        0 group)))
                             (close-port lock)
                             (loop others (- left used)))))))))


           ;;;
           ;;; Rule B -- dormant layouts.
           ;;;

           (define (dormant-layout? target layout cutoff)
             "True when nothing has written into LAYOUT for a month.  The
cheap mtime probe answers for every live project without walking the tree; the
full walk runs only for layouts that look dormant."
             (and (< (layout-activity-mtime target layout) cutoff)
                  (not (contains-file-newer-than? layout cutoff))))

           (define (remove-layout! layout bytes)
             "Delete LAYOUT, taking its markers down last so that an
interrupted removal still looks like a layout on the next run instead of
turning into unrecognisable debris."
             (let* ((markers '(".fingerprint" ".cargo-lock"))
                    (ordinary (remove (lambda (n) (member n markers))
                                      (directory-entries layout))))
               (if (%dry-run?)
                   (begin (log! "DRY" "B" "-" bytes layout)
                          (bump! 'would-delete)
                          (bump! 'would-free bytes)
                          #t)
                   (let ((ok (fold (lambda (name ok)
                                     (and (remove-tree!
                                           (string-append layout "/" name))
                                          ok))
                                   #t ordinary)))
                     (for-each (lambda (name)
                                 (let ((path (string-append layout "/" name)))
                                   (when (safe-lstat path)
                                     (remove-tree! path))))
                               markers)
                     (false-if-exception (rmdir layout))
                     (if (and ok (not (safe-lstat layout)))
                         (begin (log! "DELETE" "B" "-" bytes layout)
                                (bump! 'deleted)
                                (bump! 'freed bytes)
                                #t)
                         (begin (log! "FAIL" "B" "не удалён полностью" bytes layout)
                                (bump! 'failed)
                                #f))))))

           (define (run-rule-b targets cutoff budget force?)
             "Apply rule B.  Only layouts are removed; the root of `target'
and everything beside a layout -- package/, debian/, doc/, CACHEDIR.TAG --
is out of reach by construction."
             (let* ((all-layouts (append-map target-layouts targets))
                    (candidates
                     (append-map
                      (lambda (target)
                        (filter-map
                         (lambda (layout)
                           (and (dormant-layout? target layout cutoff)
                                (path-sane? layout)
                                (path-under? layout target)
                                (cons target layout)))
                         (target-layouts target)))
                      targets)))
               (bump! 'candidates-b (length candidates))
               (cond
                ((null? candidates) budget)
                ((and (guard-ratio %conf-dormant-guard)
                      (not force?)
                      (> (/ (length candidates) (max 1 (length all-layouts)))
                         (guard-ratio %conf-dormant-guard)))
                 (note!
                  (format #f
                          "GUARD правило B: ~a из ~a layout'ов признаны спящими, порог превышен; запуск прерван, нужен --force"
                          (length candidates) (length all-layouts)))
                 (bump! 'guard-tripped)
                 budget)
                (else
                 (let loop ((rest candidates) (left budget))
                   (cond
                    ((null? rest) left)
                    ((<= left 0)
                     (bump! 'deferred (length rest))
                     left)
                    (else
                     (match (car rest)
                       ((target . layout)
                        (let ((lock (try-lock (string-append layout
                                                             "/.cargo-lock"))))
                          (if (not lock)
                              (begin
                                (log! "SKIP-LOCKED" "B" "идёт сборка" 0 layout)
                                (bump! 'skipped-locked)
                                (loop (cdr rest) left))
                              (let ((bytes (tree-size layout)))
                                (let ((removed (remove-layout! layout bytes)))
                                  (false-if-exception (close-port lock))
                                  (loop (cdr rest)
                                        (if removed (- left 1) left)))))))))))))))


           ;;;
           ;;; Command line.
           ;;;

           (define (usage port)
             (display "\
Usage: rust-target-sweep [OPTION]...

Remove stale Rust build artefacts under the configured roots.  Only cargo
output layouts are ever touched; the root of a `target' directory, and with
it package/, debian/, doc/ and CACHEDIR.TAG, is never removed.

  --dry-run             report what would be removed (default from config)
  --execute             actually remove; conflicts with --dry-run
  --print-targets       classify candidates and exit, taking no action
  --force               proceed even if the dormant-layout guard trips
  --root DIR            walk DIR instead of the configured roots (repeatable)
  --incremental-age N   rule A threshold in days
  --dormant-age N       rule B threshold in days
  --rules LIST          comma-separated: incremental,dormant
  --help                this text

Exit: 0 clean, 1 errors during the run, 2 bad arguments, 3 guard tripped,
4 another sweep holds the lock.
" port))

           (define (option-set options key value)
             (cons (cons key value)
                   (filter (lambda (pair) (not (eq? key (car pair))))
                           options)))

           (define (parse-arguments args)
             "Return an alist, 'help, or 'usage-error."
             (let loop ((rest args)
                        (options `((mode . ,(if %conf-dry-run? 'dry 'execute))
                                   (mode-set . #f)
                                   (print? . #f)
                                   (force? . #f)
                                   (roots . ())
                                   (inc-age . ,%conf-inc-age)
                                   (dormant-age . ,%conf-dormant-age)
                                   (rules . ,%conf-rules))))
               (define (number-option tail key text)
                 (let ((value (string->number text)))
                   (if (and value (exact-integer? value) (>= value 0))
                       (loop tail (option-set options key value))
                       'usage-error)))
               (match rest
                 (() options)
                 (("--help" . _) 'help)
                 (("--dry-run" . tail)
                  ;; The two modes conflict rather than silently override:
                  ;; guessing wrong here deletes things.
                  (if (and (assq-ref options 'mode-set)
                           (eq? 'execute (assq-ref options 'mode)))
                      'usage-error
                      (loop tail (option-set (option-set options 'mode 'dry)
                                             'mode-set #t))))
                 (("--execute" . tail)
                  (if (and (assq-ref options 'mode-set)
                           (eq? 'dry (assq-ref options 'mode)))
                      'usage-error
                      (loop tail (option-set (option-set options 'mode 'execute)
                                             'mode-set #t))))
                 (("--print-targets" . tail)
                  (loop tail (option-set options 'print? #t)))
                 (("--force" . tail)
                  (loop tail (option-set options 'force? #t)))
                 (("--root" dir . tail)
                  (loop tail (option-set options 'roots
                                         (cons dir (assq-ref options 'roots)))))
                 (("--incremental-age" text . tail)
                  (number-option tail 'inc-age text))
                 (("--dormant-age" text . tail)
                  (number-option tail 'dormant-age text))
                 (("--rules" text . tail)
                  (let ((names (string-split text #\,)))
                    (if (every (lambda (name)
                                 (member name '("incremental" "dormant")))
                               names)
                        (loop tail (option-set options 'rules names))
                        'usage-error)))
                 (_ 'usage-error))))


           ;;;
           ;;; Main.
           ;;;

           (define (absolute-root path)
             "Expand and validate one configured root."
             (let ((expanded (expand-user path)))
               (cond
                ((not (string-prefix? "/" expanded))
                 (note! (format #f "REJECT корень ~s: относительный путь"
                                path))
                 #f)
                ((not (directory-not-symlink? expanded))
                 (note! (format #f "REJECT корень ~s: не каталог" expanded))
                 #f)
                ((not (= (getuid) (stat:uid (lstat expanded))))
                 (note! (format #f "REJECT корень ~s: чужой владелец" expanded))
                 #f)
                (else expanded))))

           (define (clock-sane? log-path)
             "Refuse to run when the clock disagrees with the previous run:
every decision here is made on mtime, so a skewed clock would age the whole
disk at once."
             (let ((st (safe-lstat log-path))
                   (now (current-time)))
               (or (not st)
                   (and (>= now (stat:mtime st))
                        (< (- now (stat:mtime st)) (* 366 %seconds-per-day))))))

           (define (summary-line)
             (format #f
                     "candidates A=~a B=~a | accepted=~a rejected=~a | deleted=~a would-delete=~a | freed=~a would-free=~a | skipped-locked=~a deferred=~a failed=~a errors=~a"
                     (counter 'candidates-a) (counter 'candidates-b)
                     (counter 'accepted) (counter 'rejected)
                     (counter 'deleted) (counter 'would-delete)
                     (counter 'freed) (counter 'would-free)
                     (counter 'skipped-locked) (counter 'deferred)
                     (counter 'failed) (counter 'errors)))

           (define (notify! text)
             (when %conf-notify?
               (false-if-exception
                (system* %dunstify "rust-target-sweep" text))))

           (define (print-targets roots prune-names excludes)
             "Machine-readable classification, sorted, on stdout."
             (let ((lines '()))
               (for-each
                (lambda (root)
                  (discover-targets
                   root prune-names excludes
                   (lambda (path verdict)
                     (set! lines
                           (cons (format #f "~a\t~a\t~a"
                                         (if (eq? 'accept (car verdict))
                                             "ACCEPT" "REJECT")
                                         path (cdr verdict))
                                 lines)))))
                roots)
               (for-each (lambda (line) (display line) (newline))
                         (sort lines string<?))))

           (define (sweep options)
             (let* ((now       (current-time))
                    (inc-cut   (- now (* %seconds-per-day
                                         (assq-ref options 'inc-age))))
                    (dorm-cut  (- now (* %seconds-per-day
                                         (assq-ref options 'dormant-age))))
                    (rules     (assq-ref options 'rules))
                    (prune     %conf-prune-names)
                    (excludes  (filter-map (lambda (path)
                                             (safe-canonicalize
                                              (expand-user path)))
                                           %conf-exclude))
                    (roots     (filter-map absolute-root
                                           (let ((given (assq-ref options 'roots)))
                                             (if (null? given) %conf-roots given))))
                    (targets   (append-map
                                (lambda (root)
                                  (discover-targets
                                   root prune excludes
                                   (lambda (path verdict)
                                     (if (eq? 'accept (car verdict))
                                         (begin (bump! 'accepted)
                                                (log! "ACCEPT" "-" (cdr verdict)
                                                      0 path))
                                         (begin (bump! 'rejected)
                                                (log! "REJECT" "-" (cdr verdict)
                                                      0 path))))))
                                roots)))
               (note! (format #f "start rules=~a dry-run=~a roots=~a"
                              rules (%dry-run?) roots))
               (let* ((budget (if (member "incremental" rules)
                                  (run-rule-a targets inc-cut %conf-max-deletions)
                                  %conf-max-deletions))
                      (budget (if (member "dormant" rules)
                                  (run-rule-b targets dorm-cut budget
                                              (assq-ref options 'force?))
                                  budget)))
                 (note! (string-append "summary " (summary-line)))
                 (notify! (summary-line))
                 (cond ((> (counter 'guard-tripped) 0) %exit-guard)
                       ((or (> (counter 'failed) 0)
                            (> (counter 'errors) 0)) %exit-errors)
                       (else %exit-ok)))))

           (define (main args)
             (let ((options (parse-arguments (cdr args))))
               (cond
                ((eq? options 'help) (usage (current-output-port)) %exit-ok)
                ((eq? options 'usage-error)
                 (usage (current-error-port))
                 %exit-usage)
                (else
                 (let* ((log-path (log-file-path))
                        (disable  (expand-user
                                   (or %conf-disable-file
                                       (string-append (state-directory)
                                                      "/rust-target-sweep.disable"))))
                        (lock-path (string-append (state-directory)
                                                  "/rust-target-sweep.lock")))
                   (cond
                    ((= 0 (getuid))
                     (format (current-error-port)
                             "rust-target-sweep: отказ запускаться от root~%")
                     %exit-usage)
                    ((assq-ref options 'print?)
                     ;; Classification only: no journal, no lock, no action.
                     (print-targets
                      (filter-map absolute-root
                                  (let ((given (assq-ref options 'roots)))
                                    (if (null? given) %conf-roots given)))
                      %conf-prune-names
                      (filter-map (lambda (path)
                                    (safe-canonicalize (expand-user path)))
                                  %conf-exclude))
                     %exit-ok)
                    (else
                     (mkdir-p (dirname log-path))
                     (mkdir-p (dirname lock-path))
                     (let ((lock (try-lock lock-path)))
                       (cond
                        ((not lock)
                         (format (current-error-port)
                                 "rust-target-sweep: другой прогон уже идёт~%")
                         %exit-busy)
                        ((file-exists? disable)
                         (close-port lock)
                         %exit-ok)
                        ((not (clock-sane? log-path))
                         (format (current-error-port)
                                 "rust-target-sweep: системные часы разошлись с журналом, прогон отменён~%")
                         (close-port lock)
                         %exit-guard)
                        (else
                         (truncate-log! log-path)
                         (let ((port (open-file log-path "a")))
                           (chmod log-path #o600)
                           (let ((code (parameterize
                                           ((%log-port port)
                                            (%dry-run?
                                             (eq? 'dry
                                                  (assq-ref options 'mode))))
                                         (sweep options))))
                             (close-port port)
                             (close-port lock)
                             code))))))))))))

           (exit (main (command-line))))))))


;;;
;;; Service.
;;;

(define (add-sweep-script config)
  `((".local/bin/rust-target-sweep" ,(sweep-script config))))

(define (add-sweep-activation config)
  "Create the journal's parent directory at reconfigure time: mcron gives the
job no stderr of its own, so a first run failing with ENOENT would be silent."
  (let ((log-file (home-rust-target-sweep-configuration-log-file config)))
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))
          (let* ((home (getenv "HOME"))
                 (xdg-state (or (getenv "XDG_STATE_HOME")
                                (string-append home "/.local/state")))
                 (log (or #$log-file
                          (string-append xdg-state
                                         "/log/rust-target-sweep.log"))))
            (mkdir-p (dirname log)))))))

(define (add-sweep-mcron-job config)
  "The job runs the script by its absolute store path, so mcron's empty PATH
never enters into it.  Output goes nowhere on purpose: the script keeps its
own journal, and mcron only ever sees the exit code."
  (if (home-rust-target-sweep-configuration-mcron? config)
      (list #~(job #$(home-rust-target-sweep-configuration-schedule config)
                   #$(sweep-script config)
                   "rust-target-sweep"))
      '()))

(define home-rust-target-sweep-service-type
  (service-type
   (name 'home-rust-target-sweep)
   (extensions
    (list
     (service-extension home-files-service-type add-sweep-script)
     (service-extension home-activation-service-type add-sweep-activation)
     (service-extension home-mcron-service-type add-sweep-mcron-job)))
   (default-value (home-rust-target-sweep-configuration))
   (description "Weekly removal of stale Rust build artefacts from the user's
home directory.  Only cargo output layouts, recognised by @file{.fingerprint}
or @file{.cargo-lock} inside them, are ever touched; the root of a
@file{target} directory survives, so packaged crates, @file{.deb}/@file{.rpm}
artefacts and rendered documentation are safe by construction.  Ships in
dry-run mode.")))
