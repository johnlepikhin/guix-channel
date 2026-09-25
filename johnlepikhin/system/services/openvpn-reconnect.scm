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

(define-module (johnlepikhin system services openvpn-reconnect)
  #:use-module (gnu packages linux)                ; procps
  #:use-module (gnu services)
  #:use-module (gnu services base)                 ; special-files-service-type
  #:use-module (guix gexp)
  #:export (openvpn-reconnect-service-type))

;; Make running OpenVPN clients reconnect as soon as a physical network link
;; comes up, instead of waiting for `--ping-restart' to notice that the old
;; path is gone.
;;
;; The case this is for is a VPN kept across suspend: with `vpn.persistent'
;; set, NetworkManager no longer tears the tunnel down when it takes the
;; Wi-Fi device down for sleep, so openvpn survives -- but after resume it
;; keeps talking over a session the server may have dropped, and only
;; restarts once the ping-restart timeout (about two minutes, pushed by the
;; server) runs out.  A roam to another network without sleeping looks the
;; same to openvpn.  SIGUSR1 triggers a soft restart right away; with
;; `--persist-tun' the tun device and its routes stay in place, and a server
;; that pushes `auth-token' lets the client back in without asking for the
;; password (and a one-time code) again.
;;
;; The link is the trigger rather than the elogind resume hook: that hook
;; runs before Wi-Fi has reassociated, and a restart without a network sends
;; openvpn into its resolve-retry loop with growing back-off, which ends up
;; slower than simply waiting for ping-restart.
;;
;; Only physical interfaces count (those backed by a device in sysfs): an
;; `up' of the tun device itself, a bridge or a veth pair must not restart
;; the tunnel.  Every openvpn process gets the signal, not only the ones
;; NetworkManager started.
(define %openvpn-reconnect-program
  (program-file
   "openvpn-reconnect"
   #~(begin
       (use-modules (ice-9 match))
       ;; NetworkManager passes the interface first, the action second.
       (match (command-line)
         ((_ interface "up" . _)
          (when (file-exists? (string-append "/sys/class/net/" interface
                                             "/device"))
            ;; pkill exits with 1 when no openvpn is running; that is fine.
            (system* #$(file-append procps "/bin/pkill")
                     "--signal" "USR1" "--exact" "openvpn")))
         (_ #t)))))

;; NetworkManager runs every regular, root-owned, non-group-writable
;; executable in this directory, symlinks included, so a store entry qualifies.
(define (openvpn-reconnect-special-files _)
  `(("/etc/NetworkManager/dispatcher.d/20-openvpn-reconnect"
     ,%openvpn-reconnect-program)))

(define openvpn-reconnect-service-type
  (service-type
   (name 'openvpn-reconnect)
   (extensions
    (list (service-extension special-files-service-type
                             openvpn-reconnect-special-files)))
   (default-value #f)
   (description "Send SIGUSR1 to running OpenVPN clients whenever
NetworkManager brings a physical network interface up, so that a tunnel kept
across suspend or a change of network reconnects at once rather than after the
ping-restart timeout.  For NetworkManager connections this only matters when
the connection has @code{vpn.persistent} set; otherwise NetworkManager stops
openvpn together with the underlying device.")))
