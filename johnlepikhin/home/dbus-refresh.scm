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

;;; Stop D-Bus activated daemons of the session after `guix home
;;; reconfigure', so that the bus starts them afresh on the next call.
;;;
;;; Such daemons are children of the session bus, not shepherd services,
;;; so restart-on-upgrade cannot reach them.  They live for the whole
;;; session and keep the state they loaded at startup: xdg-desktop-portal-gtk,
;;; for one, keeps the font set of the old home profile, and once those
;;; store items are collected its file chooser draws every glyph as a box.
;;;
;;; The value is a list of program names, matched against the basename of
;;; argv[0] of the user's own processes.  A dialog open at the moment of a
;;; reconfigure closes with its daemon.

(define-module (johnlepikhin home dbus-refresh)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:export (home-dbus-refresh-service-type))

(define (home-dbus-refresh-activation names)
  (if (null? names)
      #~#t
      #~(let ((names '#$(delete-duplicates names))
              (uid (getuid)))
          (define (argv0 pid)
            ;; The first NUL-terminated field of the command line.
            (false-if-exception
             (call-with-input-file (string-append "/proc/" pid "/cmdline")
               (lambda (port)
                 (let loop ((chars '()))
                   (let ((c (read-char port)))
                     (if (or (eof-object? c) (char=? c #\nul))
                         (list->string (reverse chars))
                         (loop (cons c chars)))))))))

          (define (own-pids)
            (let ((dir (opendir "/proc")))
              (let loop ((pids '()))
                (let ((entry (readdir dir)))
                  (cond ((eof-object? entry)
                         (closedir dir)
                         pids)
                        ((and (string->number entry)
                              (false-if-exception
                               (= uid (stat:uid
                                       (stat (string-append "/proc/"
                                                            entry))))))
                         (loop (cons entry pids)))
                        (else (loop pids)))))))

          ;; Only a switch of generations changes what the daemons load.
          (unless (equal? (getenv "GUIX_OLD_HOME") (getenv "GUIX_NEW_HOME"))
            (for-each
             (lambda (pid)
               (let ((program (argv0 pid)))
                 (when (and program
                            (member (basename program) names))
                   (format #t "dbus-refresh: stopping ~a (~a)~%"
                           (basename program) pid)
                   (false-if-exception
                    (kill (string->number pid) SIGTERM)))))
             (own-pids))))))

(define home-dbus-refresh-service-type
  (service-type
   (name 'home-dbus-refresh)
   (extensions
    (list (service-extension home-activation-service-type
                             home-dbus-refresh-activation)))
   (compose concatenate)
   (extend append)
   (default-value '("xdg-desktop-portal-gtk"))
   (description "Stop the given D-Bus activated daemons of the session, such
as xdg-desktop-portal-gtk, after a reconfigure, so that the bus restarts them
with the new home profile on the next call.")))
