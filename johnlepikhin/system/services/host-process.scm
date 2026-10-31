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

;;; Running programs as another user from host-side jobs that start as root,
;;; such as the mcron programs of restic maintenance and of guix-prebuild.

(define-module (johnlepikhin system services host-process)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:export (run-as-environment
            run-as
            call-as))

(define* (run-as-environment home #:optional environment)
  "The environment of a child run as a user whose home is HOME: ENVIRONMENT,
an alist, with HOME set, replacing the inherited one; or, when ENVIRONMENT is
#f, the current environment with HOME replaced.  As a list of
\"NAME=VALUE\" strings."
  (let ((entries
         (match environment
           (#f
            (filter-map (lambda (entry)
                          (match (string-index entry #\=)
                            (#f #f)
                            (at (cons (substring entry 0 at)
                                      (substring entry (+ at 1))))))
                        (environ)))
           (alist alist))))
    (map (match-lambda ((name . value) (string-append name "=" value)))
         (cons (cons "HOME" home)
               (filter (match-lambda ((name . _) (not (string=? name "HOME"))))
                       entries)))))

(define* (run-as user program args #:key environment)
  "Run PROGRAM with ARGS as USER, with the environment of
`run-as-environment'; return its exit status, 255 when it was killed, and
its standard output.  Its standard error is that of this process.  When
this process already runs as USER, privileges are left as they are."
  (let* ((pw (getpwnam user))
         (env (run-as-environment (passwd:dir pw) environment))
         (pipe (pipe))
         (pid (primitive-fork)))
    (if (zero? pid)
        (catch #t
          (lambda ()
            (close-port (car pipe))
            (dup2 (fileno (cdr pipe)) 1)
            (drop-privileges pw)
            ;; System accounts may have no home.
            (false-if-exception (chdir (passwd:dir pw)))
            (apply execle program env program args))
          (lambda _ (primitive-exit 127)))
        (begin
          (close-port (cdr pipe))
          (let ((output (get-string-all (car pipe))))
            (close-port (car pipe))
            (values (or (status:exit-val (cdr (waitpid pid))) 255)
                    output))))))

(define (drop-privileges pw)
  (unless (= (getuid) (passwd:uid pw))
    (setgroups #())
    (setgid (passwd:gid pw))
    (setuid (passwd:uid pw))))

(define (call-as user thunk)
  "Call THUNK in a child process running as USER, so that what it writes
belongs to USER and follows no link USER could have planted for root.
Return #t when THUNK returned normally."
  (let* ((pw (getpwnam user))
         (pid (primitive-fork)))
    (if (zero? pid)
        (primitive-exit
         (catch #t
           (lambda ()
             (drop-privileges pw)
             (thunk)
             0)
           (lambda (key . args)
             (format (current-error-port) "~a: ~s~%" key args)
             1)))
        (eqv? 0 (status:exit-val (cdr (waitpid pid)))))))
