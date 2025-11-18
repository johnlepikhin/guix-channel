;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Evgenii Lepikhin <johnlepikhin@gmail.com>
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

(define-module (johnlepikhin home xplugd)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (johnlepikhin packages xplugd)
  #:export (home-xplugd-configuration
            home-xplugd-service-type))

(define-record-type* <home-xplugd-configuration>
  home-xplugd-configuration make-home-xplugd-configuration
  home-xplugd-configuration?
  (package home-xplugd-configuration-package (default xplugd))
  (log-level home-xplugd-configuration-log-level (default "notice")))

(define (add-xplugd-config-file config)
  `(("xplugrc"
     ,(computed-file
       "xplugrc"
       (with-imported-modules '((guix build utils))
         #~(begin
             (use-modules (guix build utils))
             (copy-file #$(local-file "files/xplugrc")
                        #$output)
             (chmod #$output #o755)))))))

(define (add-xplugd-package config)
  (list (home-xplugd-configuration-package config)))

(define (home-xplugd-shepherd-service config)
  (let ((xplugd-pkg (home-xplugd-configuration-package config))
        (log-level (home-xplugd-configuration-log-level config)))
    (list
     (shepherd-service
      (provision '(xplugd))
      (stop #~(make-kill-destructor))
      (start #~(make-forkexec-constructor
                (list #$(file-append xplugd-pkg "/bin/xplugd")
                      "-l" #$log-level)))
      (documentation "Monitor X11 hotplug events for displays and input devices")))))

(define home-xplugd-service-type
  (service-type
   (name 'home-xplugd)
   (extensions
    (list
     (service-extension home-xdg-configuration-files-service-type
                        add-xplugd-config-file)
     (service-extension home-profile-service-type
                        add-xplugd-package)
     (service-extension home-shepherd-service-type
                        home-xplugd-shepherd-service)))
   (default-value (home-xplugd-configuration))
   (description "Run xplugd daemon to monitor X11 hotplug events and create
@file{$XDG_CONFIG_HOME/xplugrc} configuration file.")))
