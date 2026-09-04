;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023-2026 Evgenii Lepikhin <johnlepikhin@gmail.com>
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
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages xorg)
  #:use-module (gnu services admin)
  #:use-module (gnu services avahi)
  #:use-module (gnu services base)
  #:use-module (gnu services cups)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)
  #:use-module (gnu services dns)
  #:use-module (gnu services docker)
  #:use-module (gnu services linux)
  #:use-module (gnu services mcron)
  #:use-module (gnu services networking)
  #:use-module (gnu services pm)
  #:use-module (gnu services sound)
  #:use-module (gnu services ssh)
  #:use-module (gnu services virtualization)
  #:use-module (gnu services xorg)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (johnlepikhin system services brightnessctl)
  #:use-module (johnlepikhin system services polkit-network-manager)
  #:use-module (srfi srfi-1)
  #:export (make-system-services))

(define-public nonguix-signing-key
  (plain-file "non-guix.pub"
   "(public-key
     (ecc
       (curve Ed25519)
       (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)
     ))"))

(define-public bordeaux-signing-key
  (plain-file "bordeaux.guix.gnu.org.pub"
   "(public-key
     (ecc
       (curve Ed25519)
       (q #7D602902D3A2DBB83F8A0FB98602A754C5493B0B778C8D1DD4E0F41DE14DE34F#)
     ))"))

;; Port NetworkManager's dnsmasq forwards to.  Not 53: dnsmasq itself owns
;; that one on the loopback.
(define %unbound-port 5353)

;; A local recursive resolver, reached only through NetworkManager's dnsmasq.
;;
;; The networks this laptop travels through carry DPI that silently drops UDP
;; queries matching a pair (well-known public resolver, blocked name): asking
;; 8.8.8.8, 1.1.1.1, 9.9.9.9 or OpenDNS for www.google.com or www.youtube.com
;; times out, while the same question over TCP, or to any authoritative
;; server, is answered correctly.  The filter keys on the peer address, so the
;; recursive path -- roots, TLD servers, then the zone's own nameservers -- is
;; untouched, and resolving the names ourselves sidesteps the block entirely.
;; Picking a public resolver that is merely not on the list yet would not.
;;
;; No forwarders: a forwarder would put us back to trusting one address that
;; can be added to that list at any time.
(define unbound-recursive-service
  (service unbound-service-type
           (unbound-configuration
            (server
             (unbound-server
              (interface '("127.0.0.1"))
              (hide-version #t)
              (hide-identity #t)
              (tls-cert-bundle "/etc/ssl/certs/ca-certificates.crt")
              (extra-options
               `((port . ,%unbound-port)
                 ;; The default chroot is the package's own sysconfdir in the
                 ;; store, which would put the config file out of reach.
                 (chroot . "")
                 ;; Every IPv6 nameserver is unreachable on these networks,
                 ;; so trying them only adds a timeout per lookup.
                 (do-ip6 . #f)
                 ;; Recursion from the roots is slower than a forwarder on a
                 ;; cold cache; refreshing popular entries before they expire
                 ;; keeps that off the critical path.
                 (prefetch . #t))))))))

;; Hand dnsmasq the local resolver as an upstream.  NetworkManager keeps
;; programming it over D-Bus with the servers of the current connection,
;; including the domain-scoped ones a corporate VPN pushes; dnsmasq resolves
;; by longest matching domain, so VPN names keep going to the VPN's resolver
;; and only the default zone reaches unbound.  Note that this adds an upstream
;; rather than replacing the ones from DHCP -- answers are still correct, but
;; unblocked names are asked of the provider's resolver as well.
;;
;; NetworkManager only passes --conf-dir to dnsmasq when the directory is
;; already there; `dnsmasq-configuration-files' is what puts it there.
(define %dnsmasq-upstream-configuration-files
  `(("00-unbound-upstream.conf"
     ,(plain-file "00-unbound-upstream.conf"
                  (string-append "server=127.0.0.1#"
                                 (number->string %unbound-port)
                                 "\n")))))

(define lock-sessions-before-sleep
  (program-file "lock-sessions-before-sleep"
    #~(when (string=? (cadr (command-line)) "pre")
        (system* #$(file-append elogind "/bin/loginctl")
                 "lock-sessions"))))

(define* (tuned-desktop-services #:key (authorized-keys '())
                                 (substitute-urls '()))
  (cons* (modify-services %desktop-services
           (gdm-service-type config =>
                             (gdm-configuration (inherit config)
                                                (wayland? #f)))

           (pulseaudio-service-type config =>
                                    (pulseaudio-configuration (client-conf '((autospawn . no)))))

           (guix-service-type config =>
                              (guix-configuration (inherit config)
                                                  (substitute-urls (list
                                                                    ;; https://substitutes.nonguix.org is now available via IPv6 only
                                                                    "https://nonguix-proxy.ditigal.xyz"
                                                                    "https://mirror.yandex.ru/mirrors/guix"
                                                                    ;; "https://bordeaux-singapore-mirror.cbaines.net"
                                                                    ;; "https://ci.guix.gnu.org"
                                                                    ;; "https://bordeaux.guix.gnu.org/"
                                                                    ))
                                                  (discover? #t)
                                                  (extra-options
                                                   (list "--gc-keep-derivations=yes"
                                                         "--gc-keep-outputs=yes"))
                                                  (authorized-keys (append
                                                                    authorized-keys
                                                                    %default-authorized-guix-keys))))

           (upower-service-type config =>
                                (upower-configuration (inherit config)
                                                      ;; уходить в сон когда батарейки осталось на 10 минут
                                                      (time-action 600)))

           (elogind-service-type config =>
                                 (elogind-configuration (inherit config)
                                                        (handle-power-key 'suspend)
                                                        (handle-lid-switch-external-power 'suspend)
                                                        (handle-lid-switch 'suspend)
                                                        (suspend-state '("mem"))
                                                        (system-sleep-hook-files
                                                         (list lock-sessions-before-sleep))))

           (network-manager-service-type
            config =>
            (network-manager-configuration
             (inherit config)
             (dns "dnsmasq")
             (vpn-plugins
              (list
               network-manager-openvpn))
             (extra-configuration-files
              `(("default-wifi-powersave-on.conf"
                 ,(plain-file "default-wifi-powersave-on.conf"
                              "[connection]\nwifi.powersave = 2"))
                ("99-unmanaged-veth.conf"
                 ,(plain-file "99-unmanaged-veth.conf"
                              "[keyfile]\nunmanaged-devices=interface-name:veth*\n"))
                ;; Guix builds NetworkManager with rc-manager=resolvconf, and
                ;; openresolv 3.17 refuses to touch an /etc/resolv.conf whose
                ;; first line is not its own "# Generated by resolvconf"
                ;; signature.  The nscd service creates that file as a
                ;; placeholder before NetworkManager ever runs, so the very
                ;; first write loses the signature check and every later one
                ;; does too: resolv.conf stays a stub, the local dnsmasq cache
                ;; is never consulted and resolution falls back to whatever
                ;; the C library guesses.  Writing the symlink ourselves side-
                ;; steps the whole handshake -- and an existing symlink also
                ;; keeps nscd from recreating the placeholder.
                ("99-rc-manager.conf"
                 ,(plain-file "99-rc-manager.conf"
                              "[main]\nrc-manager=symlink\n"))))
             (dnsmasq-configuration-files
              %dnsmasq-upstream-configuration-files)))

           (avahi-service-type config =>
                               (avahi-configuration (ipv6? #f))))))

(define guix-pull-job
  #~(job "30 5   * * *" "guix pull"))

(define guix-gc-job
  #~(job "30 4   * * *" "guix gc -F 30G"))

;; trim free blocks on SSD
(define fstrim-job
  #~(job "40 */2   * * *" "fstrim -v /"))

(define* (make-system-services #:key (zram-size "2G")
                               (authorized-keys (list nonguix-signing-key
                                                      bordeaux-signing-key))
                               (substitute-urls (list
                                                 "https://substitutes.nonguix.org"
                                                 "https://ci.guix.gnu.org"
                                                 "https://bordeaux.guix.gnu.org/")))
  (cons*

         (set-xorg-configuration
          (xorg-configuration (modules (cons xf86-input-synaptics
                                             %default-xorg-modules))))

         (service gnome-desktop-service-type)
         (service openssh-service-type)
         (service bluetooth-service-type
                  (bluetooth-configuration (auto-enable? #t)
                                           (experimental #t)
                                           (fast-connectable? #t)))
         (simple-service 'dbus-extras dbus-root-service-type
                         (list blueman))
         (service unattended-upgrade-service-type (unattended-upgrade-configuration
                   (schedule "30 02  */3 * *")))
         (service public-backlight-brightness-service-type
                  '())
         (service hostapd-service-type
                  (hostapd-configuration (interface "wlan0_API")
                                         (ssid "My Network")
                                         (channel
                                           12)))
         ;; With intel_pstate + HWP the "powersave" governor is the normal
         ;; operating mode: it still reaches full turbo on demand, while
         ;; "performance" pins HWP min to HWP max so cores never clock down.
         ;; Combined with an explicit 2.3 GHz scaling floor that kept the
         ;; package around 86 C at idle, which in turn kept thinkfan pegged
         ;; at its top level.  Frequency floors/caps are left unset so the
         ;; governor and the energy/performance preference do the scaling.
         (service tlp-service-type
                  (tlp-configuration (cpu-scaling-governor-on-ac (list
                                                                  "powersave"))
                                     (cpu-scaling-governor-on-bat (list
                                                                   "powersave"))
                                     (energy-perf-policy-on-ac
                                       "balance_performance")
                                     (sched-powersave-on-bat? #t)
                                     (cpu-boost-on-ac? #t)
                                     (max-lost-work-secs-on-bat 180)
                                     (wifi-pwr-on-bat? #f)))
         ;; Reset TLP manual mode on AC/battery switch.  The built-in
         ;; 85-tlp.rules calls "tlp auto" which respects manual mode set
         ;; by "tlp ac"/"tlp bat" and skips profile switching.  A power
         ;; supply change generates multiple udev events (BAT0, AC, ucsi)
         ;; causing a race between "tlp auto" and "tlp start".  This rule
         ;; (priority 86) runs a wrapper that forks to background and
         ;; waits 1 second before calling "tlp start", ensuring it runs
         ;; after all "tlp auto" invocations from concurrent events.
         (udev-rules-service 'tlp-start
           (file->udev-rule "86-tlp-start.rules"
             (mixed-text-file "86-tlp-start.rules"
               "ACTION==\"change\", SUBSYSTEM==\"power_supply\", "
               "KERNEL!=\"hidpp_battery*\", "
               "RUN+=\"" (program-file "tlp-start-deferred"
                           #~(when (zero? (primitive-fork))
                               (setsid)
                               (sleep 1)
                               (execl #$(file-append tlp "/sbin/tlp")
                                      "tlp" "start")))
               "\"\n")))
         (service mcron-service-type
                  (mcron-configuration (jobs (list guix-pull-job guix-gc-job
                                                   fstrim-job))))
         (service cups-service-type
                  (cups-configuration (default-language "en")
                                      (web-interface? #t)
                                      (extensions (list cups-filters brlaser
                                                        hplip foo2zjs
                                                        foomatic-filters))))

         (service zram-device-service-type
                  (zram-device-configuration (size zram-size)
                                             (compression-algorithm 'zstd)))

         (service libvirt-service-type
                  (libvirt-configuration (unix-sock-group "libvirt")))
         (service virtlog-service-type)
         (service openvswitch-service-type)

         (service guix-publish-service-type
                  (guix-publish-configuration (host "0.0.0.0")
                                              (port 3000)
                                              (advertise? #t) ;advertise using Avahi.
                                              (cache #f)
                                              (ttl #f)))

         (service earlyoom-service-type
                  (earlyoom-configuration (prefer-regexp
                                           "(cc1(plus)?|.rustc-real|ghc|Web Content|rust-analyzer)")
                                          (avoid-regexp "xmonad")))

         (service containerd-service-type)
         (service docker-service-type)

         ;; (service fprintd-service-type)

         polkit-network-manager-service

         unbound-recursive-service

         (tuned-desktop-services #:authorized-keys authorized-keys
                                 #:substitute-urls substitute-urls)))
