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

(define-module (johnlepikhin system services utils)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services shepherd)
  #:export (make-config-file-activation
            make-simple-shepherd-service))

(define* (make-config-file-activation config-file destination-dir destination-file)
  "Create an activation gexp that copies CONFIG-FILE to DESTINATION-DIR/DESTINATION-FILE.
This is a common pattern for services that need to install configuration files
to /etc during system activation.

Example:
  (make-config-file-activation my-config \"/etc/myservice\" \"myservice.conf\")"
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))
        (mkdir-p #$destination-dir)
        (copy-file #$config-file
                   (string-append #$destination-dir "/" #$destination-file)))))

(define* (make-simple-shepherd-service
          #:key
          name
          provision
          requirement
          documentation
          package
          binary-path
          arguments
          (one-shot? #f))
  "Create a simple shepherd service definition with standard start/stop behavior.

Arguments:
  NAME: Service name (symbol)
  PROVISION: List of symbols this service provides
  REQUIREMENT: List of service dependencies
  DOCUMENTATION: Service description string
  PACKAGE: Package containing the service binary
  BINARY-PATH: Path to binary within PACKAGE (e.g., \"/sbin/daemon\")
  ARGUMENTS: List of command-line arguments for the service
  ONE-SHOT?: Boolean indicating if service runs once and exits

Returns:
  A shepherd-service record suitable for use with shepherd-root-service-type."
  (shepherd-service
   (provision provision)
   (requirement requirement)
   (documentation documentation)
   (one-shot? one-shot?)
   (start #~(make-forkexec-constructor
             (list #$(file-append package binary-path)
                   #$@arguments)))
   (stop #~(make-kill-destructor))))
