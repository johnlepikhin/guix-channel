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

;;; The selection of files to back up, and the restic wrapper that uses it.
;;;
;;; A selection is a generation directory, gen-<seconds>, holding:
;;;   include   NUL-separated absolute paths, for --files-from-raw;
;;;   exclude   restic patterns, one per line, for --exclude-file;
;;;   manifest  "key value" lines: format-version 1, generated-at <seconds>,
;;;             selector <name>, sha256 <hex of include then exclude>.
;;; It is published by pointing the symlink "current" at it, which is
;;; replaced atomically.  Whatever decides what to back up only has to
;;; publish generations this way; the backup job does not change.
;;;
;;; The wrapper stands in for restic in the jobs of
;;; `home-restic-backup-service-type'.  It sets the REST credentials, skips
;;; a backup when the server is out of reach, and gives `backup' the paths
;;; of the current selection, refusing to run with a missing, stale or
;;; damaged one.

(define-module (johnlepikhin home restic selection)
  #:use-module (gcrypt base16)
  #:use-module (gcrypt hash)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 textual-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:export (%selection-format-version
            selection-hash
            parse-manifest
            check-selection
            publish-selection
            restic-action-index
            backup-arguments
            run-restic-wrapper))

(define %selection-format-version "1")


;;;
;;; Selections.
;;;

(define (file->bytevector file)
  (call-with-input-file file get-bytevector-all #:binary #t))

(define (get-bytevector-all port)
  ((@ (ice-9 binary-ports) get-bytevector-all) port))

(define (selection-hash include exclude)
  "Hex SHA-256 of the bytevectors INCLUDE then EXCLUDE."
  (let ((empty (make-bytevector 0)))
    (bytevector->base16-string
     (sha256 (bytevector-append (if (eof-object? include) empty include)
                                (if (eof-object? exclude) empty exclude))))))

(define (bytevector-append a b)
  (let ((out (make-bytevector (+ (bytevector-length a)
                                 (bytevector-length b)))))
    (bytevector-copy! a 0 out 0 (bytevector-length a))
    (bytevector-copy! b 0 out (bytevector-length a) (bytevector-length b))
    out))

(define (parse-manifest text)
  "An alist of the \"key value\" lines of TEXT."
  (filter-map (lambda (line)
                (match (string-index line #\space)
                  (#f #f)
                  (i (cons (substring line 0 i)
                           (string-trim-both (substring line (+ i 1)))))))
              (string-split text #\newline)))

(define* (check-selection directory now #:key (max-age (* 2 86400)))
  "Check the selection generation DIRECTORY.  Return its hash, or raise an
error saying what is wrong with it."
  (define (fail fmt . args)
    (error (apply format #f (string-append "backup selection ~a: " fmt)
                  directory args)))
  (define (file name) (string-append directory "/" name))
  (unless (file-exists? (file "manifest"))
    (fail "no manifest"))
  (let* ((manifest (parse-manifest
                    (call-with-input-file (file "manifest") get-string-all)))
         (field (lambda (key) (assoc-ref manifest key)))
         (generated (and (field "generated-at")
                         (string->number (field "generated-at")))))
    (unless (equal? (field "format-version") %selection-format-version)
      (fail "unsupported format version ~s" (field "format-version")))
    (unless generated
      (fail "no generation time"))
    (when (> (- now generated) max-age)
      (fail "stale, generated ~a hours ago by ~a"
            (quotient (- now generated) 3600) (field "selector")))
    (unless (and (file-exists? (file "include"))
                 (> (stat:size (stat (file "include"))) 0))
      (fail "nothing to back up"))
    (let ((hash (selection-hash (file->bytevector (file "include"))
                                (if (file-exists? (file "exclude"))
                                    (file->bytevector (file "exclude"))
                                    (eof-object)))))
      (unless (equal? hash (field "sha256"))
        (fail "damaged, its files do not match the manifest"))
      hash)))

(define* (publish-selection root roots excludes selector now
                            #:key (keep 3))
  "Publish a generation under ROOT with the paths ROOTS and the patterns
EXCLUDES, made by SELECTOR at NOW, and keep the KEEP newest generations."
  (let* ((name (string-append "gen-" (number->string now)))
         (dir (string-append root "/" name))
         (include (string->utf8 (string-concatenate
                                 (map (lambda (path) (string-append path "\0"))
                                      roots))))
         (exclude (string->utf8 (string-concatenate
                                 (map (lambda (pattern)
                                        (string-append pattern "\n"))
                                      excludes))))
         (write-file (lambda (file bv)
                       (call-with-output-file (string-append dir "/" file)
                         (lambda (port) (put-bytevector port bv))
                         #:binary #t))))
    (mkdir-p* root)
    (mkdir-p* dir)
    (write-file "include" include)
    (write-file "exclude" exclude)
    (call-with-output-file (string-append dir "/manifest")
      (lambda (port)
        (format port "format-version ~a~%generated-at ~a~%selector ~a~%\
sha256 ~a~%"
                %selection-format-version now selector
                (selection-hash include exclude))))
    (let ((tmp (string-append root "/current.tmp")))
      (false-if-exception (delete-file tmp))
      (symlink name tmp)
      (rename-file tmp (string-append root "/current")))
    ;; Older generations go, but not one a running backup may still read.
    (for-each (lambda (old)
                (delete-tree (string-append root "/" old)))
              (drop-generations (generations root) keep))
    dir))

(define (put-bytevector port bv)
  ((@ (ice-9 binary-ports) put-bytevector) port bv))

(define (generations root)
  "Names of the generations under ROOT, newest first."
  (sort (filter (lambda (name) (string-prefix? "gen-" name))
                (or (false-if-exception (scandir* root)) '()))
        (lambda (a b)
          (> (string->number (substring a 4))
             (string->number (substring b 4))))))

(define (drop-generations names keep)
  (if (> (length names) keep) (drop names keep) '()))

(define (scandir* dir)
  (let ((d (opendir dir)))
    (let loop ((names '()))
      (let ((name (readdir d)))
        (if (eof-object? name)
            (begin (closedir d) names)
            (loop (if (member name '("." "..")) names (cons name names))))))))

(define (mkdir-p* dir)
  (unless (file-exists? dir)
    (mkdir-p* (dirname dir))
    (mkdir dir #o700)))

(define (delete-tree path)
  (if (eq? 'directory (stat:type (lstat path)))
      (begin
        (for-each (lambda (name) (delete-tree (string-append path "/" name)))
                  (scandir* path))
        (rmdir path))
      (delete-file path)))


;;;
;;; The wrapper.
;;;

;; Global options of restic that take a separate value.
(define %valued-options
  '("-r" "--repo" "-p" "--password-file" "--password-command"
    "--repository-file" "--cacert" "--cache-dir" "-o" "--option"
    "--retry-lock" "--tls-client-cert" "--key-hint" "--limit-upload"
    "--limit-download" "--pack-size" "--compression"
    "--stuck-request-timeout"))

(define (restic-action-index args)
  "The index in ARGS of the restic command, such as \"backup\", or #f."
  (let loop ((args args) (i 0))
    (match args
      (() #f)
      (((? (lambda (a) (member a %valued-options))) value . rest)
       (loop rest (+ i 2)))
      (((? (lambda (a) (string-prefix? "-" a))) . rest)
       (loop rest (+ i 1)))
      ((_ . _) i))))

(define (backup-arguments args index generation hash)
  "ARGS with the flags of the selection GENERATION, of HASH, after the
command at INDEX."
  (append (list-head args (+ index 1))
          (list "--files-from-raw" (string-append generation "/include")
                "--exclude-file" (string-append generation "/exclude")
                "--tag" (string-append "selection:" (string-take hash 8))
                ;; The paths of a selection may change; grouping by host
                ;; keeps the parent snapshot, and the retention, stable.
                "--group-by" "host")
          (drop args (+ index 1))))

(define (read-credentials file)
  "The user and the password in FILE, \"user:password\" on its first line."
  (let ((line (string-trim-right (call-with-input-file file read-line))))
    (match (string-index line #\:)
      (#f (error "credentials file must hold user:password" file))
      (i (values (substring line 0 i) (substring line (+ i 1)))))))

(define (reachable? host port seconds)
  "Whether a TCP connection to HOST:PORT succeeds within SECONDS."
  (catch #t
    (lambda ()
      (let* ((ai (car (getaddrinfo host (number->string port)
                                   AI_NUMERICSERV)))
             (sock (socket (addrinfo:fam ai) SOCK_STREAM 0)))
        (fcntl sock F_SETFL (logior O_NONBLOCK (fcntl sock F_GETFL)))
        (catch 'system-error
          (lambda () (connect sock (addrinfo:addr ai)))
          (lambda args
            (unless (= (system-error-errno args) EINPROGRESS)
              (apply throw args))))
        (let* ((ready (select '() (list sock) '() seconds))
               (ok? (and (pair? (cadr ready))
                         (zero? (getsockopt sock SOL_SOCKET SO_ERROR)))))
          (close-port sock)
          ok?)))
    (const #f)))

(define (run-restic-wrapper restic credentials reachability selection
                            max-age args)
  "Run RESTIC with ARGS, see the commentary of this module."
  (when credentials
    (call-with-values (lambda () (read-credentials credentials))
      (lambda (user password)
        (setenv "RESTIC_REST_USERNAME" user)
        (setenv "RESTIC_REST_PASSWORD" password))))
  (let* ((index (restic-action-index args))
         (backup? (and index (string=? (list-ref args index) "backup"))))
    (when (and backup? reachability
               (not (reachable? (car reachability) (cdr reachability) 5)))
      (format #t "restic backup skipped: ~a:~a is out of reach~%"
              (car reachability) (cdr reachability))
      (exit 0))
    (let ((args
           (if (and backup? selection)
               (let* ((current (string-append selection "/current"))
                      (generation
                       (catch 'system-error
                         (lambda ()
                           (string-append selection "/" (readlink current)))
                         (lambda _
                           (format (current-error-port)
                                   "backup selection ~a: none published~%"
                                   current)
                           (exit 1))))
                      (hash (catch 'misc-error
                              (lambda ()
                                (check-selection generation (current-time)
                                                 #:max-age max-age))
                              (lambda (key subr message args . rest)
                                (format (current-error-port) "~?~%"
                                        message args)
                                (exit 1)))))
                 (backup-arguments args index generation hash))
               args)))
      (apply execl restic restic args))))
