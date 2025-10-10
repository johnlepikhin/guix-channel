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

(define-module (johnlepikhin system services thinkfan)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (gnu packages linux)
  #:export (thinkfan-service-type
            thinkfan-configuration))

(define-record-type* <thinkfan-configuration>
  thinkfan-configuration
  make-thinkfan-configuration
  thinkfan-configuration?
  (config-file          thinkfan-configuration-config-file
                        (default #f))
  (update-interval      thinkfan-configuration-update-interval
                        (default 1)))

(define (thinkfan-activation config)
  (let ((config-file (thinkfan-configuration-config-file config)))
    (with-imported-modules '((guix build utils))
      (let ((etc-dir "/etc/thinkfan")
            (thinkfan.yaml "/etc/thinkfan/thinkfan.yaml"))
        #~(begin
            (use-modules (guix build utils))
            (mkdir-p #$etc-dir)
            (copy-file #$config-file #$thinkfan.yaml))))))

(define-public (thinkfan-shepherd-service config)
  (let ((config-file (thinkfan-configuration-config-file config))
        (update-interval (thinkfan-configuration-update-interval config)))
    (list (shepherd-service
           (provision '(thinkfan))
           (requirement '(udev))
           (documentation "Thinkfan service")
           (start #~(make-forkexec-constructor
                     (list #$(file-append thinkfan "/bin/thinkfan")
                           "-c" "/etc/thinkfan/thinkfan.yaml"
                           "-n"
                           "-s" #$(number->string update-interval))))
           (stop #~(make-kill-destructor))))))

(define-public thinkfan-service-type
  (service-type
   (name 'thinkfan)
   (description "Thinkfan service")
   (extensions
    (list
     (service-extension activation-service-type
                        thinkfan-activation)
     (service-extension profile-service-type
                        (const (list thinkfan)))
     (service-extension shepherd-root-service-type
                        thinkfan-shepherd-service)))))
