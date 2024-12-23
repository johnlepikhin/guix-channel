;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Evgenii Lepikhin <johnlepikhin@gmail.com>
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

(define-module (johnlepikhin system os)
  #:use-module (nongnu packages linux)
  #:use-module (nongnu packages video)
  #:use-module (nongnu system linux-initrd)
  #:use-module (gnu)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu packages linux)
  #:use-module (gnu system)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system locale)
  #:use-module (gnu system pam)
  #:use-module (gnu system nss)
  #:use-module (guix channels)
  #:export (make-desktop-operating-system
            default-kernel-arguments))

(define default-kernel-arguments '("modprobe.blacklist=pcspkr,snd_pcsp"))

(define* (make-desktop-operating-system
          #:key
          hostname
          users
          services
          packages
          file-systems
          (linux-kernel linux-6.10)
          (kernel-loadable-modules (list v4l2loopback-linux-module))
          (firmware
           (list
            sof-firmware
            linux-firmware
            realtek-firmware
            iwlwifi-firmware
            amdgpu-firmware
            radeon-firmware
            ibt-hw-firmware
            i915-firmware
            intel-microcode
            amd-microcode
            intel-media-driver/nonfree))
          (timezone "Europe/Moscow")
          (sudoers-file-extras "")
          (kernel-arguments default-kernel-arguments)
          (mapped-devices '()))
  (operating-system
   (kernel linux-kernel)
   (kernel-loadable-modules kernel-loadable-modules)
   (kernel-arguments kernel-arguments)
   (initrd microcode-initrd)
   (firmware firmware)
   (locale "en_US.utf8")
   (locale-definitions
    (list (locale-definition (source "en_US")
                             (name "en_US.UTF-8"))
          (locale-definition (source "ru_RU")
                             (name "ru_RU.UTF-8"))))
   (timezone timezone)
   (keyboard-layout (keyboard-layout "us" "altgr-intl"))
   (host-name hostname)
   (users users)
   (packages packages)
   (services services)
   ;; resolve .local hostnames with mDNS
   (name-service-switch %mdns-host-lookup-nss)
   (sudoers-file
    (plain-file "etc-sudoers-config"
                (string-append "root ALL=(ALL) ALL
%wheel ALL=(ALL) ALL
" sudoers-file-extras)))
   (bootloader
    (bootloader-configuration
     (bootloader grub-efi-bootloader)
     (targets (list "/boot/efi"))
     (keyboard-layout keyboard-layout)
     (theme (grub-theme
             (gfxmode '("1440x900x32" "auto"))))))
   (mapped-devices mapped-devices)
   (file-systems file-systems)))
