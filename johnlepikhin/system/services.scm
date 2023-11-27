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

(define-module (johnlepikhin system services)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages xorg)
  #:use-module (gnu services admin)
  #:use-module (gnu services authentication)
  #:use-module (gnu services avahi)
  #:use-module (gnu services base)
  #:use-module (gnu services cups)
  #:use-module (gnu services databases)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)
  #:use-module (gnu services dns)
  #:use-module (gnu services docker)
  #:use-module (gnu services linux)
  #:use-module (gnu services mcron)
  #:use-module (gnu services networking)
  #:use-module (gnu services pm)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services sound)
  #:use-module (gnu services ssh)
  #:use-module (gnu services virtualization)
  #:use-module (gnu services xorg)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:export (make-system-services
            postgresql-dev-service))

(define nonguix-signing-key
  "(public-key
     (ecc
       (curve Ed25519)
       (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)
     ))")

(define bordeaux-signing-key
  "(public-key
     (ecc
       (curve Ed25519)
       (q #7D602902D3A2DBB83F8A0FB98602A754C5493B0B778C8D1DD4E0F41DE14DE34F#)
     ))")

(define-public postgresql-dev-service
  (service postgresql-service-type
           (postgresql-configuration
            (postgresql postgresql-15)
            (extension-packages (list postgis))
            (config-file
             (postgresql-config-file
              (log-destination "stderr")
              (hba-file
               (plain-file "pg_hba.conf"
                           "
local   all all         trust
host    all all 127.0.0.1/32    md5
host    all all ::1/128     md5")))))))

(define-public tuned-desktop-services
  (cons*
   (modify-services
    %desktop-services

    ;; (delete gdm-service-type)

    (pulseaudio-service-type
     config =>
     (pulseaudio-configuration
      (client-conf '((autospawn . no)))))

    (guix-service-type
     config =>
     (guix-configuration
      (inherit config)
      (substitute-urls
       (list
        "https://substitutes.nonguix.org"
        "https://ci.guix.gnu.org"
        "https://bordeaux.guix.gnu.org/"))
      (discover? #t)
      (authorized-keys
       (append
        (list
         (plain-file "non-guix.pub" nonguix-signing-key)
         (plain-file "bordeaux.guix.gnu.org.pub" bordeaux-signing-key))
        %default-authorized-guix-keys))))

    (upower-service-type
     config =>
     (upower-configuration
      (inherit config)
      ;; уходить в сон когда батарейки осталось на 10 минут
      (time-action 600)))

    (elogind-service-type
     config =>
     (elogind-configuration
      (inherit config)
      (handle-power-key 'suspend)
      (handle-lid-switch-external-power 'suspend)
      (handle-lid-switch 'suspend)))

    (network-manager-service-type
     config => (network-manager-configuration
                (inherit config)
                (dns "dnsmasq")
                (vpn-plugins
                 (list network-manager-openvpn))))

    (avahi-service-type
     config =>
     (avahi-configuration
      (ipv6? #f))))))

(define (public-backlight-brightness-service _)
  (list (shepherd-service
         (provision '(public-backlight-brightness))
         (documentation "Grant R/W to /sys/class/backlight/*/brightness for all users")
         (one-shot? #t)
         (start #~(lambda _ (system "/run/current-system/profile/bin/chmod 666 /sys/class/backlight/*/brightness"))))))

(define (add-brightnessctl-package config)
  (list brightnessctl))

(define public-backlight-brightness-service-type
  (service-type
   (name 'public-backlight-brightness)
   (description "Grant R/W to /sys/class/backlight/*/brightness for all users")
   (extensions
    (list
     (service-extension
      profile-service-type add-brightnessctl-package)
     (service-extension
      shepherd-root-service-type public-backlight-brightness-service)))))

(define guix-pull-job
  #~(job "30 5   * * *"
         "guix pull"))

(define guix-gc-job
  #~(job "30 4   * * *"
         "guix gc -F 30G"))

;; trim free blocks on SSD
(define fstrim-job
  #~(job "40 2   * * *" "fstrim -v /"))

(define* (make-system-services
          #:key
          (zram-size "2G"))
  (cons*

   (set-xorg-configuration
    (xorg-configuration
     (modules (cons xf86-input-synaptics %default-xorg-modules))))

   (service gnome-desktop-service-type)
   (service openssh-service-type)
   (service bluetooth-service-type
            (bluetooth-configuration
             (auto-enable? #t)
             (experimental #t)))
   (simple-service 'dbus-extras
                   dbus-root-service-type
                   (list blueman))
   (service unattended-upgrade-service-type
            (unattended-upgrade-configuration
             (schedule "30 02 * * */3")))
   (service public-backlight-brightness-service-type '())
   (service hostapd-service-type
            (hostapd-configuration
             (interface "wlan0_API")
             (ssid "My Network")
             (channel 12)))
   (service tlp-service-type
            (tlp-configuration
             (cpu-scaling-governor-on-ac (list "performance"))
             (cpu-scaling-governor-on-bat (list "powersave"))
             (sched-powersave-on-bat? #f)
             (cpu-boost-on-ac? #t)
             (max-lost-work-secs-on-bat 180)
             (cpu-scaling-min-freq-on-bat (* 400 1000))
             (cpu-scaling-max-freq-on-bat (* 800 1000))
             (cpu-scaling-min-freq-on-ac (* 2300 1000))
             (cpu-scaling-max-freq-on-ac (* 4500 1000))))
   (service mcron-service-type
            (mcron-configuration
             (jobs
              (list
               guix-pull-job
               guix-gc-job
               fstrim-job))))
   (service cups-service-type
            (cups-configuration
             (default-language "en")
             (web-interface? #t)
             (extensions
              (list cups-filters brlaser hplip foo2zjs foomatic-filters))))

   (service zram-device-service-type
            (zram-device-configuration
             (size zram-size)
             (compression-algorithm 'zstd)))

   (service libvirt-service-type
            (libvirt-configuration
             (unix-sock-group "libvirt")))
   (service virtlog-service-type)
   (service openvswitch-service-type)
   (service qemu-binfmt-service-type
            (qemu-binfmt-configuration
             (platforms
              (fold delete %qemu-platforms
                    (lookup-qemu-platforms "i386" "x86_64")))))

   (service guix-publish-service-type
            (guix-publish-configuration
             (host "0.0.0.0")
             (port 3000)
             (advertise? #t) ;advertise using Avahi.
             (cache #f)
             (ttl #f)))

   (service earlyoom-service-type
            (earlyoom-configuration
             (prefer-regexp "(cc1(plus)?|.rustc-real|ghc|Web Content|rust-analyzer)")
             (avoid-regexp "xmonad")))

   (service docker-service-type)

   ;; (service fprintd-service-type)

   tuned-desktop-services))
