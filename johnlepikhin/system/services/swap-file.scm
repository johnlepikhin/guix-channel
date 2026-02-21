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

(define-module (johnlepikhin system services swap-file)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages base)
  #:use-module (gnu packages linux)
  #:export (swap-file-service-type
            swap-file-configuration
            swap-file-configuration?
            swap-file-configuration-path
            swap-file-configuration-size-mb))

(define-record-type* <swap-file-configuration>
  swap-file-configuration make-swap-file-configuration
  swap-file-configuration?
  (path    swap-file-configuration-path    (default "/var/swapfile"))
  (size-mb swap-file-configuration-size-mb (default 4096)))

(define (swap-file-shepherd-service config)
  (let ((path    (swap-file-configuration-path config))
        (size-mb (swap-file-configuration-size-mb config)))
    (list (shepherd-service
           (provision '(swap-file))
           (requirement '(file-systems))
           (documentation "Manage swap file lifecycle: create, mkswap, swapon/swapoff")
           (one-shot? #f)
           (start #~(lambda ()
                      (let ((dd      #$(file-append coreutils "/bin/dd"))
                            (chmod   #$(file-append coreutils "/bin/chmod"))
                            (mkswap  #$(file-append util-linux "/sbin/mkswap"))
                            (swapon  #$(file-append util-linux "/sbin/swapon"))
                            (stat    stat))
                        (define (file-size-bytes file)
                          (catch #t
                            (lambda () (stat:size (stat file)))
                            (lambda _ #f)))
                        (let ((expected-bytes (* #$size-mb 1024 1024))
                              (current-size (file-size-bytes #$path)))
                          (when (or (not current-size)
                                    (not (= current-size expected-bytes)))
                            (system* dd "if=/dev/zero"
                                     (string-append "of=" #$path)
                                     "bs=1M"
                                     (string-append "count=" (number->string #$size-mb)))
                            (system* chmod "600" #$path)
                            (system* mkswap #$path)))
                        (zero? (system* swapon #$path)))))
           (stop #~(lambda _
                     (let ((swapoff #$(file-append util-linux "/sbin/swapoff")))
                       (zero? (system* swapoff #$path))
                       #f)))))))

(define swap-file-service-type
  (service-type
   (name 'swap-file)
   (description "Swap file management service.
Creates a swap file of the specified size if it does not exist or has the wrong
size, then activates it with swapon.  On stop, deactivates with swapoff.")
   (extensions
    (list
     (service-extension shepherd-root-service-type
                        swap-file-shepherd-service)))))
