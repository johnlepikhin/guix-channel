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

(define-module (johnlepikhin home mpd)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module ((gnu packages mpd) #:hide (rmpc))
  #:use-module ((johnlepikhin packages rmpc) #:select (rmpc))
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:export (home-mpd-configuration
            home-mpd-configuration?
            home-mpd-service-type))

(define-record-type* <home-mpd-configuration>
  home-mpd-configuration make-home-mpd-configuration
  home-mpd-configuration?
  (package            home-mpd-configuration-package
                      (default mpd))
  (music-directory    home-mpd-configuration-music-directory
                      (default "~/Music"))
  (playlist-directory home-mpd-configuration-playlist-directory
                      (default "~/.local/share/mpd/playlists"))
  (db-file            home-mpd-configuration-db-file
                      (default "~/.local/share/mpd/database"))
  (state-file         home-mpd-configuration-state-file
                      (default "~/.local/share/mpd/state"))
  (log-file           home-mpd-configuration-log-file
                      (default "~/.local/state/log/mpd.log"))
  (port               home-mpd-configuration-port
                      (default 6600))
  (audio-output-type  home-mpd-configuration-audio-output-type
                      (default "pulse"))
  (audio-output-name  home-mpd-configuration-audio-output-name
                      (default "PulseAudio"))
  (extra-options      home-mpd-configuration-extra-options
                      (default '())))

(define (serialize-mpd-configuration config)
  (let ((music-directory    (home-mpd-configuration-music-directory config))
        (playlist-directory (home-mpd-configuration-playlist-directory config))
        (db-file            (home-mpd-configuration-db-file config))
        (state-file         (home-mpd-configuration-state-file config))
        (log-file           (home-mpd-configuration-log-file config))
        (port               (home-mpd-configuration-port config))
        (audio-output-type  (home-mpd-configuration-audio-output-type config))
        (audio-output-name  (home-mpd-configuration-audio-output-name config))
        (extra-options      (home-mpd-configuration-extra-options config)))
    (string-append
     "music_directory     \"" music-directory "\"\n"
     "playlist_directory  \"" playlist-directory "\"\n"
     "db_file             \"" db-file "\"\n"
     "state_file          \"" state-file "\"\n"
     "log_file            \"" log-file "\"\n"
     "port                \"" (number->string port) "\"\n"
     "auto_update         \"yes\"\n"
     "\n"
     "audio_output {\n"
     "    type    \"" audio-output-type "\"\n"
     "    name    \"" audio-output-name "\"\n"
     "}\n"
     (if (null? extra-options)
         ""
         (string-append "\n" (string-join extra-options "\n") "\n")))))

(define (add-mpd-config-files config)
  `(("mpd/mpd.conf"
     ,(plain-file "mpd.conf" (serialize-mpd-configuration config)))))

(define (add-mpd-shepherd-service config)
  (let ((package (home-mpd-configuration-package config)))
    (list
     (shepherd-service
      (provision '(mpd))
      (requirement '(pipewire-pulse))
      (documentation "Run the Music Player Daemon.")
      (start #~(make-forkexec-constructor
                (list #$(file-append package "/bin/mpd")
                      "--no-daemon"
                      (string-append
                       (or (getenv "XDG_CONFIG_HOME")
                           (string-append (getenv "HOME") "/.config"))
                       "/mpd/mpd.conf"))
                #:log-file (string-append
                            (or (getenv "XDG_STATE_HOME")
                                (string-append (getenv "HOME")
                                               "/.local/state"))
                            "/log/mpd.log")))
      (stop #~(make-kill-destructor)))
     (shepherd-service
      (provision '(mpdris2))
      (requirement '(mpd dbus))
      (documentation "Run mpDris2 MPRIS bridge for MPD.")
      (start #~(make-forkexec-constructor
                (list #$(file-append mpdris2 "/bin/mpDris2"))
                #:log-file (string-append
                            (or (getenv "XDG_STATE_HOME")
                                (string-append (getenv "HOME")
                                               "/.local/state"))
                            "/log/mpdris2.log")))
      (stop #~(make-kill-destructor))))))

(define (add-mpd-packages config)
  (list (home-mpd-configuration-package config)
        rmpc
        mpdris2))

(define (add-mpd-activation config)
  (let ((music-directory    (home-mpd-configuration-music-directory config))
        (playlist-directory (home-mpd-configuration-playlist-directory config))
        (db-file            (home-mpd-configuration-db-file config))
        (state-file         (home-mpd-configuration-state-file config))
        (log-file           (home-mpd-configuration-log-file config)))
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))
          (let ((home (getenv "HOME")))
            (define (resolve path)
              (if (string-prefix? "~/" path)
                  (string-append home "/" (substring path 2))
                  path))
            (for-each (lambda (dir) (mkdir-p (resolve dir)))
                      (list #$music-directory
                            #$playlist-directory))
            (for-each (lambda (file) (mkdir-p (dirname (resolve file))))
                      (list #$db-file
                            #$state-file
                            #$log-file)))))))

(define home-mpd-service-type
  (service-type
   (name 'home-mpd)
   (extensions
    (list
     (service-extension home-xdg-configuration-files-service-type
                        add-mpd-config-files)
     (service-extension home-shepherd-service-type
                        add-mpd-shepherd-service)
     (service-extension home-profile-service-type
                        add-mpd-packages)
     (service-extension home-activation-service-type
                        add-mpd-activation)))
   (default-value (home-mpd-configuration))
   (description "Run the Music Player Daemon as a user service.")))
