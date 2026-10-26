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

;;; Module: (johnlepikhin home remote-build)
;;;
;;; `rb': build on a remote host what is edited locally.
;;;
;;;   rb cargo clippy -- -D warnings
;;;
;;; run from inside a configured project syncs the working tree to the host
;;; and runs the command there, in a long-lived Docker container of the
;;; project, with the output and exit code of the command.  Build caches
;;; (target/, node_modules/, ~/.gradle) stay warm on the host.
;;;
;;; Sync.  The files git would see -- tracked and untracked, without the
;;; ignored ones -- are copied with rsync; files that left that list since
;;; the previous sync are deleted on the host; ignored files there are
;;; never touched.  .git follows when its refs or index change.  The tree
;;; is mounted in the container at its local absolute path, so paths in
;;; compiler messages are the local ones.
;;;
;;; Container.  Built from an image or from a Dockerfile of the project, it
;;; runs as the host user with the host network, the project's $HOME and a
;;; /tmp shared by all projects (`make openapi-dump' in one project feeds
;;; `api:update' in another).  It is recreated when its configuration or
;;; image changes -- but never under a running command.  Setup commands
;;; run once per container; a failed setup is retried by the next command.
;;;
;;; Commands without a terminal (pipes, editors, agents) get a heartbeat:
;;; when rb or the connection goes away, the remote command is killed
;;; instead of holding locks such as target/.cargo-lock.
;;;
;;; Runbook:
;;;   rb status            container, setup, services, running commands
;;;   rb ps / rb kill [ID] commands running in the container
;;;   rb sync --full       resync after an interrupted or confused sync
;;;   rb restart           recreate the container (kills running commands)
;;;   rb pull              fetch newer images of tags such as :latest
;;;   rb down [--purge]    remove containers and images [and host data]
;;;   ~/<remote-root>/<project>/rb-setup.log on the host: image builds and
;;;   setup output.
;;;
;;; Errors of rb itself exit with 75 and an "rb:" prefix.
;;;
;;; Docker's nightly `system prune --all' on a host removes stopped
;;; containers with their images; rb therefore never stops a container.

(define-module (johnlepikhin home remote-build)
  #:use-module (gcrypt hash)
  #:use-module (gnu home services)
  #:use-module (gnu packages base)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages version-control)
  #:use-module (gnu services)
  #:use-module (guix base16)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:export (remote-build-dockerfile
            remote-build-dockerfile?

            remote-build-service
            remote-build-service?

            remote-build-project
            remote-build-project?

            home-remote-build-configuration
            home-remote-build-configuration?
            home-remote-build-service-type))

;; Part of every configuration hash: bump it when the container layout set
;; up by rbh.sh changes, so that existing containers are recreated.
(define %rb-schema-version "1")


;;;
;;; Configuration.
;;;

(define (valid-name name)
  (unless (and (string? name)
               (not (string-null? name))
               (string-every (lambda (c)
                               (or (char-set-contains? char-set:digit c)
                                   (char-set-contains?
                                    (string->char-set "abcdefghijklmnopqrstuvwxyz-_.")
                                    c)))
                             name))
    (error "remote-build: invalid name, use [a-z0-9._-]:" name))
  name)

(define (absolute-path path)
  (unless (and (string? path) (string-prefix? "/" path))
    (error "remote-build: an absolute path is required:" path))
  path)

(define-record-type* <remote-build-dockerfile>
  remote-build-dockerfile make-remote-build-dockerfile
  remote-build-dockerfile?
  ;; Path of the Dockerfile inside the project.
  (file       remote-build-dockerfile-file)
  ;; Build context inside the project, or #f for an empty one: right for a
  ;; Dockerfile without COPY, and it spares sending a huge tree.
  (context    remote-build-dockerfile-context (default #f))
  ;; Alist of --build-arg values, e.g. '(("RUST_VER" . "1.92")).
  (build-args remote-build-dockerfile-build-args (default '())))

(define-record-type* <remote-build-service>
  remote-build-service make-remote-build-service
  remote-build-service?
  (name        remote-build-service-name (sanitize valid-name))
  (image       remote-build-service-image)                  ;string
  ;; List of (HOST-PORT CONTAINER-PORT) or (ADDRESS HOST-PORT
  ;; CONTAINER-PORT); the address defaults to 127.0.0.1.
  (ports       remote-build-service-ports (default '()))
  (environment remote-build-service-environment (default '())) ;alist
  ;; Arguments of the image's command, e.g. '("-c" "max_connections=200").
  (arguments   remote-build-service-arguments (default '()))
  ;; Further `docker run' options, e.g. '("--shm-size=2g").
  (options     remote-build-service-options (default '())))

(define-record-type* <remote-build-project>
  remote-build-project make-remote-build-project
  remote-build-project?
  (name        remote-build-project-name (sanitize valid-name))
  ;; Local working tree; the container sees it at the same path.
  (directory   remote-build-project-directory (sanitize absolute-path))
  ;; An image reference, preferably with a digest, or a
  ;; <remote-build-dockerfile>.
  (image       remote-build-project-image)
  ;; Shell commands run once per container: as root, then as the user.
  (root-setup  remote-build-project-root-setup (default #f))
  (setup       remote-build-project-setup (default #f))
  ;; Alist; "$HOME" in a value stands for the container's home.
  (environment remote-build-project-environment (default '()))
  ;; Alist of local files ("~/" allowed) to paths relative to the
  ;; container's home, copied on every sync.
  (files       remote-build-project-files (default '()))
  ;; Directories on the host mounted read-only at the same path, for
  ;; secrets placed there by hand.
  (secret-directories remote-build-project-secret-directories
                      (default '()))
  ;; Docker limits, e.g. "32g" and "24"; #f for none.
  (memory      remote-build-project-memory (default #f))
  (cpus        remote-build-project-cpus (default #f))
  (options     remote-build-project-options (default '()))
  (services    remote-build-project-services (default '()))
  ;; Default patterns for `rb fetch': relative to the project, or under
  ;; /tmp.  "*" also matches "/".
  (artifacts   remote-build-project-artifacts (default '())))

(define-record-type* <home-remote-build-configuration>
  home-remote-build-configuration make-home-remote-build-configuration
  home-remote-build-configuration?
  ;; Host as ssh knows it: an alias of ~/.ssh/config or user@host.
  (host        home-remote-build-configuration-host)
  ;; Directory of the projects on the host, relative to its $HOME.
  (remote-root home-remote-build-configuration-remote-root
               (default "remote-build"))
  ;; Passed to every ssh, also for rsync; ControlPath comes from
  ;; ~/.ssh/config.
  (ssh-options home-remote-build-configuration-ssh-options
               (default '("-o" "ControlMaster=auto"
                          "-o" "ControlPersist=10m"
                          "-o" "ServerAliveInterval=15"
                          "-o" "ConnectTimeout=10")))
  (projects    home-remote-build-configuration-projects (default '())))


;;;
;;; Project specification, read by rbh.sh on the host.
;;;

(define (shell-quote str)
  (string-append "'" (string-join (string-split str #\') "'\\''") "'"))

(define (bash-variable name value)
  (string-append name "=" (shell-quote value) "\n"))

(define (bash-array name items)
  (string-append name "=(" (string-join (map shell-quote items) " ") ")\n"))

(define (sha256-hex str)
  (bytevector->base16-string (sha256 (string->utf8 str))))

(define (alist->assignments alist)
  (map (match-lambda ((key . value) (string-append key "=" value))) alist))

(define (service-arguments service)
  "The `docker run' arguments of SERVICE before its image."
  (append
   (append-map (match-lambda
                 ((host-port container-port)
                  (list "-p" (format #f "127.0.0.1:~a:~a"
                                     host-port container-port)))
                 ((address host-port container-port)
                  (list "-p" (format #f "~a:~a:~a"
                                     address host-port container-port))))
               (remote-build-service-ports service))
   (append-map (lambda (assignment) (list "-e" assignment))
               (alist->assignments
                (remote-build-service-environment service)))
   (remote-build-service-options service)))

(define (service-specification index service)
  (let* ((prefix (format #f "SVC_~a_" index))
         (body (string-append
                (bash-variable (string-append prefix "IMAGE")
                               (remote-build-service-image service))
                (bash-array (string-append prefix "ARGS")
                            (service-arguments service))
                (bash-array (string-append prefix "CMD")
                            (remote-build-service-arguments service)))))
    (string-append body
                   (bash-variable (string-append prefix "HASH")
                                  (sha256-hex (string-append
                                               %rb-schema-version body))))))

(define (project-specification project)
  "The project as bash assignments.  CONFIG_HASH covers what the container
is made of; services carry hashes of their own."
  (let* ((image (remote-build-project-image project))
         (dockerfile? (remote-build-dockerfile? image))
         (container
          (string-append
           (bash-variable "SCHEMA" %rb-schema-version)
           (bash-variable "SRC_PATH" (remote-build-project-directory project))
           (bash-variable "IMAGE" (if dockerfile? "" image))
           (bash-variable "DOCKERFILE"
                          (if dockerfile?
                              (remote-build-dockerfile-file image)
                              ""))
           (bash-variable "CONTEXT"
                          (or (and dockerfile?
                                   (remote-build-dockerfile-context image))
                              ""))
           (bash-array "BUILD_ARGS"
                       (if dockerfile?
                           (alist->assignments
                            (remote-build-dockerfile-build-args image))
                           '()))
           (bash-array "ENVIRONMENT"
                       (alist->assignments
                        (remote-build-project-environment project)))
           (bash-array "DOCKER_OPTIONS"
                       (append
                        (match (remote-build-project-memory project)
                          (#f '())
                          (memory (list (string-append "--memory=" memory)
                                        (string-append "--memory-swap="
                                                       memory))))
                        (match (remote-build-project-cpus project)
                          (#f '())
                          (cpus (list (string-append "--cpus=" cpus))))
                        (remote-build-project-options project)))
           (bash-variable "ROOT_SETUP"
                          (or (remote-build-project-root-setup project) ""))
           (bash-variable "SETUP"
                          (or (remote-build-project-setup project) ""))
           (bash-array "SECRET_DIRECTORIES"
                       (map absolute-path
                            (remote-build-project-secret-directories
                             project)))))
         (services (remote-build-project-services project)))
    (string-append
     container
     (bash-variable "CONFIG_HASH" (sha256-hex container))
     (bash-array "SERVICES" (map remote-build-service-name services))
     (string-concatenate
      (map service-specification (iota (length services)) services)))))


;;;
;;; The rb command.
;;;

(define (project-gexp project)
  #~(list #$(remote-build-project-name project)
          #$(remote-build-project-directory project)
          #$(plain-file (string-append "rb-spec-"
                                       (remote-build-project-name project))
                        (project-specification project))
          '#$(remote-build-project-artifacts project)
          '#$(remote-build-project-files project)))

(define (rb-program config)
  (program-file
   "rb"
   #~(begin
       (use-modules (ice-9 binary-ports)
                    (ice-9 ftw)
                    (ice-9 match)
                    (ice-9 rdelim)
                    (ice-9 textual-ports)
                    (rnrs bytevectors)
                    (srfi srfi-1)
                    (srfi srfi-11)
                    (srfi srfi-26))

       (define %ssh #$(file-append openssh-sans-x "/bin/ssh"))
       (define %rsync #$(file-append rsync "/bin/rsync"))
       (define %git #$(file-append git-minimal "/bin/git"))
       (define %tar #$(file-append tar "/bin/tar"))
       (define %host #$(home-remote-build-configuration-host config))
       (define %root #$(home-remote-build-configuration-remote-root config))
       (define %ssh-options
         '#$(home-remote-build-configuration-ssh-options config))
       (define %helper #$(local-file "remote-build/rbh.sh"))
       (define %wrap #$(local-file "remote-build/rb-wrap.sh"))
       ;; (NAME DIRECTORY SPEC ARTIFACTS FILES)
       (define %projects
         (list #$@(map project-gexp
                       (home-remote-build-configuration-projects config))))

       (define %ex-tempfail 75)
       (define %verbose? #f)

       (define (fail fmt . args)
         (format (current-error-port) "rb: ~?~%" fmt args)
         (exit %ex-tempfail))

       (define (note fmt . args)
         (format (current-error-port) "rb: ~?~%" fmt args))

       (define (now)
         (match (gettimeofday)
           ((seconds . microseconds) (+ seconds (/ microseconds 1e6)))))

       (define (timed label thunk)
         (let ((start (now)))
           (call-with-values thunk
             (lambda results
               (when %verbose?
                 (note "~a: ~,2fs" label (- (now) start)))
               (apply values results)))))

       (define (shell-quote str)
         (string-append "'" (string-join (string-split str #\') "'\\''")
                        "'"))

       ;;
       ;; Processes.  Pipes are close-on-exec, so a child never holds the
       ;; write end of its own stdin open.
       ;;

       (define (make-pipe)
         (let ((ends (pipe)))
           (fcntl (car ends) F_SETFD FD_CLOEXEC)
           (fcntl (cdr ends) F_SETFD FD_CLOEXEC)
           ends))

       (define* (spawn args #:key stdin stdout)
         (let ((pid (primitive-fork)))
           (if (zero? pid)
               (begin
                 (catch #t
                   (lambda ()
                     (when stdin (dup2 (port->fdes stdin) 0))
                     (when stdout (dup2 (port->fdes stdout) 1))
                     (apply execl (car args) args))
                   (const #f))
                 (primitive-_exit 127))
               pid)))

       (define (status->code status)
         (or (status:exit-val status)
             (+ 128 (or (status:term-sig status) 0))))

       (define* (run args #:key input capture?)
         "Run ARGS, writing the string INPUT to its stdin.  Return its exit
code and, with CAPTURE?, its stdout."
         (let* ((in (and input (make-pipe)))
                (out (and capture? (make-pipe)))
                (pid (spawn args
                            #:stdin (and in (car in))
                            #:stdout (and out (cdr out)))))
           (when in (close-port (car in)))
           (when out (close-port (cdr out)))
           (when in
             (catch 'system-error
               (lambda () (put-bytevector (cdr in) (string->utf8 input)))
               (const #f))
             (close-port (cdr in)))
           (let ((output (and out
                              (let ((bytes (get-bytevector-all (car out))))
                                (close-port (car out))
                                (if (eof-object? bytes)
                                    ""
                                    (utf8->string bytes))))))
             (values (status->code (cdr (waitpid pid))) output))))

       (define (run-code . args)
         (call-with-values (lambda () (apply run args))
           (lambda (code . ignored) code)))

       (define (run/check what args . rest)
         (call-with-values (lambda () (apply run args rest))
           (lambda (code . output)
             (unless (zero? code)
               (fail "~a failed (exit ~a)" what code))
             (if (null? output) #t (car output)))))

       (define (string-set items)
         (let ((table (make-hash-table (length items))))
           (for-each (cut hash-set! table <> #t) items)
           table))

       (define (without items excluded)
         "ITEMS that are not in EXCLUDED, in order."
         (let ((table (string-set excluded)))
           (remove (cut hash-ref table <>) items)))

       (define (split-nul str)
         (remove string-null? (string-split str #\nul)))

       (define (join-nul items)
         (string-concatenate (map (cut string-append <> "\0") items)))

       ;;
       ;; Remote side.
       ;;

       (define (ssh-arguments . options)
         (append (list %ssh) %ssh-options options (list %host)))

       (define (ssh-command-string)
         (string-join (cons %ssh %ssh-options) " "))

       (define (remote-path . parts)
         (string-join (cons %root parts) "/"))

       (define (helper-remote) (remote-path ".rb" (basename %helper)))
       (define (wrap-remote) (remote-path ".rb" (basename %wrap)))
       (define (spec-remote project)
         (match project
           ((name _ spec . _)
            (remote-path name (string-append ".rb-spec-" (basename spec))))))

       (define (helper-command project command . args)
         (string-join
          (append (list (string-append "RB_HOME="
                                       (shell-quote (getenv "HOME")))
                        "bash" (shell-quote (helper-remote))
                        (shell-quote %root) (shell-quote (first project))
                        (shell-quote (spec-remote project))
                        (shell-quote (wrap-remote))
                        (shell-quote command))
                  (map shell-quote args))
          " "))

       (define* (helper project command #:key input capture? (args '()))
         (call-with-values
             (lambda ()
               (run (append (ssh-arguments)
                            (list (apply helper-command project command
                                         args)))
                    #:input input #:capture? capture?))
           (lambda (code . output)
             (when (= code 255)
               (fail "ssh to ~a failed" %host))
             (apply values code output))))

       (define (helper/check project command . rest)
         (call-with-values (lambda () (apply helper project command rest))
           (lambda (code . ignored)
             (unless (zero? code)
               (fail "~a on ~a failed (exit ~a)" command %host code)))))

       (define (helper-code project command . args)
         (call-with-values (lambda () (helper project command #:args args))
           (lambda (code . ignored) code)))

       (define (upload local remote)
         (let ((target (shell-quote remote)))
           (run/check (string-append "upload of " remote)
                      (append (ssh-arguments)
                              (list (format #f "mkdir -p ~a && cat > ~a.tmp && mv ~a.tmp ~a"
                                            (shell-quote (dirname remote))
                                            target target target)))
                      #:input (call-with-input-file local get-string-all))))

       (define (ensure-uploaded project)
         "Copy the helper, rb-wrap and the specification to the host unless
it has them already: their names carry the store hash."
         (let ((files `((,%helper . ,(helper-remote))
                        (,%wrap . ,(wrap-remote))
                        (,(third project) . ,(spec-remote project)))))
           (match (run-code (append (ssh-arguments)
                               (list (string-join
                                      (map (lambda (file)
                                             (string-append
                                              "test -f "
                                              (shell-quote (cdr file))))
                                           files)
                                      " && "))))
             (0 #t)
             (255 (fail "ssh to ~a failed" %host))
             (_ (for-each (match-lambda
                            ((local . remote) (upload local remote)))
                          files)))))

       (define (call-with-project-lock project thunk)
         "Hold the project's lock on the host, a flock kept by an ssh session
whose stdin stays open, while THUNK runs."
         (let* ((in (make-pipe))
                (out (make-pipe))
                (pid (spawn (append (ssh-arguments)
                                    (list (helper-command project "lock")))
                            #:stdin (car in) #:stdout (cdr out))))
           (close-port (car in))
           (close-port (cdr out))
           (unless (equal? (read-line (car out)) "locked")
             (close-port (cdr in))
             (waitpid pid)
             (fail "could not take the project lock on ~a" %host))
           (dynamic-wind
             (const #t)
             thunk
             (lambda ()
               (close-port (cdr in))
               (close-port (car out))
               (waitpid pid)))))

       ;;
       ;; Sync.
       ;;

       (define (git-files top . arguments)
         (split-nul (run/check "git ls-files"
                               (append (list %git "-C" top "ls-files" "-z")
                                       arguments)
                               #:capture? #t)))

       (define (local-file-list top)
         "What git sees in TOP, minus files deleted from the working tree but
not from the index."
         (let ((deleted (git-files top "--deleted")))
           (filter (lambda (file)
                     (false-if-exception
                      (lstat (string-append top "/" file))))
                   (without (git-files top "--cached" "--others"
                                       "--exclude-standard")
                            deleted))))

       (define (git-fingerprint top)
         "A digest of what changes in .git when refs, objects or the index
change -- not of its contents, which would cost a full read."
         (let ((git-dir (string-append top "/.git")))
           (if (not (file-is-directory? git-dir))
               ""
               (let ((stats '()))
                 (define (add! path)
                   (let ((st (false-if-exception (lstat path))))
                     (set! stats
                           (cons (if st
                                     (format #f "~a ~a ~a" path
                                             (stat:mtime st) (stat:size st))
                                     path)
                                 stats))))
                 (for-each (lambda (name)
                             (add! (string-append git-dir "/" name)))
                           '("HEAD" "index" "packed-refs"))
                 (for-each (lambda (directory)
                             (let ((path (string-append git-dir "/"
                                                        directory)))
                               (when (file-exists? path)
                                 (ftw path (lambda (file stat flag)
                                             (add! file)
                                             #t)))))
                           '("refs" "objects/pack"))
                 ;; A new loose object changes the mtime of its directory.
                 (for-each (lambda (name)
                             (add! (string-append git-dir "/objects/" name)))
                           (or (scandir (string-append git-dir "/objects")
                                        (lambda (name)
                                          (= 2 (string-length name))))
                               '()))
                 (number->string
                  (string-hash (string-join (sort stats string<?) "\n")))))))

       (define (remote-state project)
         "Return the host's clock offset, the git fingerprint and the
manifest of the last sync."
         (let* ((before (now))
                (output (call-with-values
                            (lambda () (helper project "state" #:capture? #t))
                          (lambda (code output)
                            (unless (zero? code)
                              (fail "reading the host state failed"))
                            output)))
                (after (now)))
           (match (string-split output #\newline)
             ((clock fingerprint . rest)
              (values (- (or (string->number clock) after)
                         (/ (+ before after) 2))
                      fingerprint
                      (split-nul (string-join rest "\n"))))
             (_ (fail "unexpected host state")))))

       (define (sync! project top full?)
         (match project
           ((name directory spec artifacts files)
            (let-values (((offset fingerprint manifest)
                          (timed "state" (cut remote-state project))))
              (when (> (abs offset) 1)
                (note "clock of ~a is off by ~,1fs: builds may miss changes"
                      %host offset))
              (let* ((current (timed "list" (cut local-file-list top)))
                     (previous (if full?
                                   (append
                                    manifest
                                    (call-with-values
                                        (lambda ()
                                          (helper project "list"
                                                  #:capture? #t))
                                      (lambda (code output)
                                        (split-nul output))))
                                   manifest))
                     (gone (delete-duplicates (without previous current)))
                     (destination (string-append %host ":"
                                                 (remote-path name "src")
                                                 "/")))
                (timed "rsync"
                       (lambda ()
                         (let-values (((code ignored)
                                       (run (list %rsync "-a" "--from0"
                                                  "--files-from=-"
                                                  "-e" (ssh-command-string)
                                                  "./" destination)
                                            #:input (join-nul current))))
                           ;; 24: a file vanished while being copied.
                           (unless (memv code '(0 24))
                             (fail "rsync failed (exit ~a)" code)))))
                (unless (null? gone)
                  (when %verbose?
                    (note "deleting ~a files on the host" (length gone)))
                  (helper/check project "delete" #:input (join-nul gone)))
                (let ((local-fingerprint (git-fingerprint top)))
                  (cond
                   ((file-exists? (string-append top "/.git/index.lock"))
                    (note "git is busy here (.git/index.lock): .git not synced"))
                   ((and (not full?)
                         (string=? local-fingerprint fingerprint)))
                   ((file-is-directory? (string-append top "/.git"))
                    (timed "rsync .git"
                           (lambda ()
                             (run/check "rsync of .git"
                                        (list %rsync "-a" "--delete"
                                              "--exclude=*.lock"
                                              "-e" (ssh-command-string)
                                              ".git/"
                                              (string-append destination
                                                             ".git/")))))))
                  (helper/check project "commit"
                                #:args (list local-fingerprint)
                                #:input (join-nul current)))
                (for-each
                 (match-lambda
                   ((local . target)
                    (let ((local (if (string-prefix? "~/" local)
                                     (string-append (getenv "HOME")
                                                    (string-drop local 1))
                                     local)))
                      (when (file-exists? local)
                        (run/check (string-append "copy of " local)
                                   (list %rsync "-a" "--mkpath"
                                         "-e" (ssh-command-string)
                                         local
                                         (string-append
                                          %host ":"
                                          (remote-path name "home" target))))))))
                 files))))))

       ;;
       ;; Commands.
       ;;

       (define (ensure project force?)
         (let ((code (apply helper-code project "ensure"
                            (if force? '("force") '()))))
           (unless (zero? code)
             (exit code))))

       (define (prepare project top sync? full? force?)
         (ensure-uploaded project)
         (call-with-project-lock
          project
          (lambda ()
            (when sync? (sync! project top full?))
            (timed "ensure" (cut ensure project force?)))))

       (define (heartbeat-run args)
         "Run ARGS with a newline on its stdin every few seconds, which keeps
the remote command alive; return its exit code."
         (sigaction SIGPIPE SIG_IGN)
         (let* ((in (make-pipe))
                (pid (spawn args #:stdin (car in))))
           (close-port (car in))
           (let loop ((tick 0))
             (match (waitpid pid WNOHANG)
               ((0 . _)
                (when (zero? (modulo tick 25))
                  (catch 'system-error
                    (lambda ()
                      (put-char (cdr in) #\newline)
                      (force-output (cdr in)))
                    (const #f)))
                (usleep 200000)
                (loop (+ tick 1)))
               ((_ . status)
                (false-if-exception (close-port (cdr in)))
                (status->code status))))))

       (define (execute project workdir command)
         (let* ((tty? (and (isatty? (current-input-port))
                           (isatty? (current-output-port))))
                (id (format #f "~a-~a" (getpid) (car (gettimeofday))))
                (remote (apply helper-command project "exec"
                               (if tty? "tty" "watch") workdir id command)))
           (if tty?
               (apply execl %ssh (append (ssh-arguments "-t")
                                         (list remote)))
               (match (timed "exec"
                             (cut heartbeat-run
                                  (append (ssh-arguments) (list remote))))
                 (255 (fail "ssh to ~a failed" %host))
                 (code (exit code))))))

       (define (fetch project top patterns)
         (match project
           ((name directory . _)
            (let* ((patterns (if (null? patterns)
                                 (fourth project)
                                 patterns))
                   (classified
                    (map (lambda (pattern)
                           (cond
                            ((string-prefix? "/tmp/" pattern)
                             (cons 'tmp (string-drop pattern 5)))
                            ((string-prefix? (string-append directory "/")
                                             pattern)
                             (cons 'src (string-drop
                                         pattern
                                         (+ 1 (string-length directory)))))
                            ((string-prefix? "/" pattern)
                             (fail "~a is neither in the project nor in /tmp"
                                   pattern))
                            (else (cons 'src pattern))))
                         patterns)))
              (when (null? patterns)
                (fail "nothing to fetch: give paths or configure artifacts"))
              (for-each
               (match-lambda
                 ((kind . destination)
                  (let ((selected (filter-map (match-lambda
                                                ((k . pattern)
                                                 (and (eq? k kind) pattern)))
                                              classified)))
                    (unless (null? selected)
                      (let* ((pipe-ends (make-pipe))
                             (ssh (spawn (append
                                          (ssh-arguments)
                                          (list (apply helper-command project
                                                       "fetch"
                                                       (symbol->string kind)
                                                       selected)))
                                         #:stdout (cdr pipe-ends)))
                             (untar (begin
                                      (close-port (cdr pipe-ends))
                                      (spawn (list %tar "-C" destination
                                                   "-xvf" "-")
                                             #:stdin (car pipe-ends)))))
                        (close-port (car pipe-ends))
                        (let ((ssh-code (status->code (cdr (waitpid ssh))))
                              (tar-code (status->code (cdr (waitpid untar)))))
                          (unless (and (zero? ssh-code) (zero? tar-code))
                            (fail "fetch failed (ssh ~a, tar ~a)"
                                  ssh-code tar-code))))))))
               `((src . ,top) (tmp . "/tmp")))))))

       ;;
       ;; Command line.
       ;;

       (define %subcommands
         '("sync" "shell" "fetch" "status" "ps" "kill" "restart" "pull"
           "down"))

       (define (usage)
         (display "\
Usage: rb [-p PROJECT] [--no-sync] [-v] [--] COMMAND [ARG]...
       rb [-p PROJECT] [-v] -c 'SHELL COMMANDS'
       rb [-p PROJECT] SUBCOMMAND

Run COMMAND in the project's container on the build host, after syncing
the working tree.  Options end at the first other word; a \"--\" there is
dropped, later ones belong to COMMAND.  Without a terminal, COMMAND reads
/dev/null and is killed when rb goes away.  A sync while another command
builds changes that build's input.

Subcommands:
  sync [--full]    sync only; --full also deletes stray host files
  shell            interactive shell in the container
  fetch [PATH]...  copy files back (default: the project's artifacts)
  status           container, setup, services, running commands
  ps, kill [ID]    list or stop running commands
  restart          recreate the container, killing running commands
  pull             fetch newer images for the next command
  down [--purge]   remove containers and images [and all host data]
To run a program named like a subcommand: rb -- NAME.
Errors of rb itself exit with 75.
")
         (exit 0))

       (define (git-toplevel)
         (call-with-values
             (lambda ()
               (run (list %git "rev-parse" "--show-toplevel") #:capture? #t))
           (lambda (code output)
             (and (zero? code) (string-trim-right output)))))

       (define (canonical path)
         (or (false-if-exception (canonicalize-path path)) path))

       (define (find-project name)
         (or (if name
                 (find (lambda (p) (string=? (first p) name)) %projects)
                 (let ((top (git-toplevel)))
                   (and top
                        (find (lambda (p)
                                (string=? (canonical (second p))
                                          (canonical top)))
                              %projects))))
             (fail "~a; configured: ~a"
                   (if name
                       (string-append "no project " name)
                       "not inside a configured project")
                   (string-join (map first %projects) ", "))))

       (define (container-workdir directory)
         "The container's working directory for the current one: the same
path, since the tree is mounted at its local path."
         (let ((cwd (canonical (getcwd)))
               (top (canonical directory)))
           (if (or (string=? cwd top)
                   (string-prefix? (string-append top "/") cwd))
               (string-append directory (string-drop cwd (string-length top)))
               directory)))

       (define (main args)
         (let loop ((args args) (name #f) (sync? #t))
           (match args
             (("-p" project . rest) (loop rest project sync?))
             (("--no-sync" . rest) (loop rest name #f))
             (("-v" . rest) (set! %verbose? #t) (loop rest name sync?))
             (((or "-h" "--help") . _) (usage))
             (()
              (usage))
             (_
              (let* ((project (find-project name))
                     (directory (second project))
                     (workdir (container-workdir directory)))
                (chdir directory)
                (match args
                  (("-c" script)
                   (prepare project directory sync? #f #f)
                   (execute project workdir (list "sh" "-c" script)))
                  (("--" . command)
                   (prepare project directory sync? #f #f)
                   (execute project workdir command))
                  (((? (cut member <> %subcommands) subcommand) . rest)
                   (match (cons subcommand rest)
                     (("sync" . options)
                      (prepare project directory #t
                               (member "--full" options) #f))
                     (("shell")
                      (prepare project directory sync? #f #f)
                      (execute project workdir
                               '("sh" "-c" "if command -v bash > /dev/null; then exec bash; else exec sh; fi")))
                     (("fetch" . patterns)
                      (ensure-uploaded project)
                      (fetch project directory patterns))
                     (("restart")
                      (prepare project directory #f #f #t))
                     (((and command (or "status" "ps" "kill" "pull"))
                       . rest)
                      (ensure-uploaded project)
                      (exit (apply helper-code project command rest)))
                     (("down" . options)
                      (ensure-uploaded project)
                      (exit (apply helper-code project "down"
                                   (if (member "--purge" options)
                                       '("purge")
                                       '()))))
                     (_ (usage))))
                  (command
                   (prepare project directory sync? #f #f)
                   (execute project workdir command))))))))

       (false-if-exception (setlocale LC_ALL ""))
       ;; Notes must interleave with the output of ssh and rsync.
       (setvbuf (current-error-port) 'line)
       (main (cdr (command-line))))))


;;;
;;; Service.
;;;

(define (add-rb-script config)
  `((".local/bin/rb" ,(rb-program config))))

(define home-remote-build-service-type
  (service-type
   (name 'home-remote-build)
   (extensions
    (list (service-extension home-files-service-type add-rb-script)))
   (description "Install @command{rb}, which runs commands of configured
projects in Docker containers on a remote build host after syncing the
working tree there.")))
