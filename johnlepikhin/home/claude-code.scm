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

(define-module (johnlepikhin home claude-code)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (johnlepikhin packages ai)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:export (home-claude-code-configuration
            home-claude-code-service-type))

(define-record-type* <home-claude-code-configuration>
  home-claude-code-configuration make-home-claude-code-configuration
  home-claude-code-configuration?
  ;; The claude-code package providing bin/claude.
  (package          home-claude-code-configuration-package
                    (default claude-code))
  ;; Directory, relative to $HOME, in which to place the `claude' symlink.
  ;; A leading "~/" is accepted and stripped.  home-files-service-type always
  ;; resolves paths relative to the home directory.
  (symlink-directory home-claude-code-configuration-symlink-directory
                     (default ".local/bin")))

(define (relative-directory directory)
  "Normalise DIRECTORY into a path relative to $HOME, dropping a leading
\"~/\" and any leading slash, as required by home-files-service-type."
  (let ((stripped (cond ((string-prefix? "~/" directory) (substring directory 2))
                        ((string-prefix? "/"  directory) (substring directory 1))
                        (else directory))))
    (string-trim-right stripped #\/)))

(define (add-claude-code-package config)
  (list (home-claude-code-configuration-package config)))

(define (add-claude-code-files config)
  (let* ((package   (home-claude-code-configuration-package config))
         (directory (relative-directory
                     (home-claude-code-configuration-symlink-directory config)))
         (link      (string-append directory "/claude")))
    ;; The symlink is part of the home generation: it is GC-rooted, removed
    ;; when the service is dropped, and reconfigure refuses to clobber an
    ;; unmanaged file at the same path instead of deleting it silently.
    (list (list link (file-append package "/bin/claude")))))

(define home-claude-code-service-type
  (service-type
   (name 'home-claude-code)
   (extensions
    (list
     (service-extension home-profile-service-type
                        add-claude-code-package)
     (service-extension home-files-service-type
                        add-claude-code-files)))
   (default-value (home-claude-code-configuration))
   (description "Install claude-code and symlink its @code{claude} binary
into a user bin directory (default @file{~/.local/bin}).")))
