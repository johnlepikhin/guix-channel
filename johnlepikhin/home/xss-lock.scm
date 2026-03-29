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

(define-module (johnlepikhin home xss-lock)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (johnlepikhin home xsession)
  #:use-module (johnlepikhin packages xidlehook)
  #:export (home-xss-lock-configuration
            home-xss-lock-service-type))

(define-record-type* <home-xss-lock-configuration>
  home-xss-lock-configuration make-home-xss-lock-configuration
  home-xss-lock-configuration?
  (locker home-xss-lock-locker (default "slock"))
  (dim-timeout home-xss-lock-dim-timeout (default 300))
  (lock-timeout home-xss-lock-lock-timeout (default 30))
  (pre-lock-commands home-xss-lock-pre-lock-commands (default '()))
  (post-unlock-commands home-xss-lock-post-unlock-commands (default '())))

(define lock-screen-script-path ".config/xss-lock/lock-screen.sh")
(define idle-daemon-script-path ".config/xss-lock/idle-daemon.sh")

(define (make-lock-screen-script config)
  (let ((locker (home-xss-lock-locker config))
        (pre-cmds (home-xss-lock-pre-lock-commands config))
        (post-cmds (home-xss-lock-post-unlock-commands config)))
    (computed-file
     "lock-screen.sh"
     #~(begin
         (with-output-to-file #$output
           (lambda _
             (display
              (string-append
               "#!/bin/sh\n"
               "\n"
               "trap 'dunstify -u critical \"lock-screen\" \"Lock failed\"' ERR 2>/dev/null || true\n"
               "\n"
               "PRIMARY_DISPLAY=\"$(xrandr | awk '/ primary/{print $1}')\"\n"
               "if [ -z \"$PRIMARY_DISPLAY\" ]; then\n"
               "    PRIMARY_DISPLAY=\"$(xrandr | awk '/ connected/{print $1; exit}')\"\n"
               "fi\n"
               "\n"
               "# Restore brightness in case it was dimmed\n"
               "if [ -n \"$PRIMARY_DISPLAY\" ]; then\n"
               "    xrandr --output \"$PRIMARY_DISPLAY\" --brightness 1\n"
               "fi\n"
               "\n"
               "# Pre-lock commands\n"
               #$@(map (lambda (cmd) (string-append cmd " || true\n")) pre-cmds)
               "\n"
               "# Start locker in background\n"
               #$locker " &\n"
               "LOCKER_PID=$!\n"
               "\n"
               "# Give locker time to grab the screen\n"
               "sleep 0.5\n"
               "\n"
               "# Release the sleep lock so the system can suspend\n"
               "if [ -n \"$XSS_SLEEP_LOCK_FD\" ]; then\n"
               "    exec {XSS_SLEEP_LOCK_FD}>&-\n"
               "fi\n"
               "\n"
               "# Wait for locker to exit (user unlocked)\n"
               "wait $LOCKER_PID\n"
               "\n"
               "sleep 0.2\n"
               "\n"
               "# Post-unlock commands\n"
               #$@(map (lambda (cmd) (string-append cmd "\n")) post-cmds)
               "\n"))))
         (chmod #$output #o755)))))

(define (make-idle-daemon-script config)
  (let ((dim-timeout (number->string (home-xss-lock-dim-timeout config)))
        (lock-timeout (number->string (home-xss-lock-lock-timeout config))))
    (computed-file
     "idle-daemon.sh"
     #~(begin
         (with-output-to-file #$output
           (lambda _
             (display
              (string-append
               "#!/bin/sh\n"
               "\n"
               "PRIMARY_DISPLAY=\"$(xrandr | awk '/ primary/{print $1}')\"\n"
               "if [ -z \"$PRIMARY_DISPLAY\" ]; then\n"
               "    PRIMARY_DISPLAY=\"$(xrandr | awk '/ connected/{print $1; exit}')\"\n"
               "fi\n"
               "\n"
               "if [ -z \"$PRIMARY_DISPLAY\" ]; then\n"
               "    echo \"No display found\"\n"
               "    exit 1\n"
               "fi\n"
               "\n"
               "exec xidlehook --not-when-fullscreen \\\n"
               "    --timer " #$dim-timeout
               " \"xrandr --output \\\"$PRIMARY_DISPLAY\\\" --brightness .5\""
               " \"xrandr --output \\\"$PRIMARY_DISPLAY\\\" --brightness 1\" \\\n"
               "    --timer " #$lock-timeout
               " \"$HOME/" #$lock-screen-script-path "\""
               " ''\n"))))
         (chmod #$output #o755)))))

(define (add-xss-lock-files config)
  `((,lock-screen-script-path ,(make-lock-screen-script config))
    (,idle-daemon-script-path ,(make-idle-daemon-script config))))

(define (add-xss-lock-packages config)
  (list xss-lock xidlehook))

(define (add-xss-lock-xsession config)
  (list
   (xsession-component
    (command (string-append "xss-lock -l -- $HOME/" lock-screen-script-path " &"))
    (priority 15))
   (xsession-component
    (command (string-append "$HOME/" idle-daemon-script-path " &"))
    (priority 16))))

(define home-xss-lock-service-type
  (service-type
   (name 'home-xss-lock)
   (extensions
    (list
     (service-extension home-files-service-type
                        add-xss-lock-files)
     (service-extension home-profile-service-type
                        add-xss-lock-packages)
     (service-extension home-xsession-service-type
                        add-xss-lock-xsession)))
   (default-value (home-xss-lock-configuration))
   (description "Run xss-lock and xidlehook for screen locking with dim and idle timeout.")))
