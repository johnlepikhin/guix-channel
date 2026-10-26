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

;;; Host names that resolve differently depending on where a laptop is.
;;;
;;; A server at home is reached by its LAN address on the home network and by
;;; its VPN address elsewhere.  Whenever NetworkManager changes the network,
;;; a dispatcher script picks the first matching location and writes its
;;; names into a hosts file that NetworkManager's dnsmasq serves
;;; (`addn-hosts'); dnsmasq is told to reload and nscd to forget cached
;;; answers.  With no matching location the names do not resolve at all,
;;; which lets clients such as `guix substitute' skip the host at once
;;; instead of waiting for a connection timeout.
;;;
;;; NetworkManager must run with dns=dnsmasq, and the file returned by
;;; `network-location-hosts-dnsmasq-configuration' must be among its
;;; `dnsmasq-configuration-files'.

(define-module (johnlepikhin system services network-location-hosts)
  #:use-module (gnu packages base)                 ; glibc (nscd)
  #:use-module (gnu packages gnome)                ; network-manager (nmcli)
  #:use-module (gnu packages linux)                ; iproute
  #:use-module (gnu packages networking)           ; iputils
  #:use-module (gnu services)
  #:use-module (gnu services base)                 ; special-files
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:export (network-location
            network-location?
            network-location-hosts-configuration
            network-location-hosts-configuration?
            network-location-hosts-dnsmasq-configuration
            network-location-hosts-service-type))

;; A location matches when every condition that is set holds.
(define-record-type* <network-location>
  network-location
  make-network-location
  network-location?
  (name             network-location-name)           ;string, for the log
  ;; The IPv4 default gateway, and its MAC address so that any other network
  ;; that happens to use the same private addresses does not match.
  (gateway-address  network-location-gateway-address
                    (default #f))
  (gateway-mac      network-location-gateway-mac
                    (default #f))
  ;; Name of a NetworkManager connection that must be active, e.g. a VPN.
  (connection       network-location-connection
                    (default #f))
  ;; Alist of host names to addresses.
  (hosts            network-location-hosts
                    (default '())))

(define-record-type* <network-location-hosts-configuration>
  network-location-hosts-configuration
  make-network-location-hosts-configuration
  network-location-hosts-configuration?
  ;; Tried in order; the first match wins.
  (locations   network-location-hosts-configuration-locations
               (default '()))
  (hosts-file  network-location-hosts-configuration-hosts-file
               (default "/run/network-location/hosts")))

(define (location->sexp location)
  "Return LOCATION as a list for the dispatcher script, checking that it has
a condition."
  (let ((name (network-location-name location))
        (address (network-location-gateway-address location))
        (mac (network-location-gateway-mac location))
        (connection (network-location-connection location)))
    (unless (or address connection)
      (error "network-location has no condition:" name))
    (when (and mac (not address))
      (error "network-location: gateway-mac without gateway-address:" name))
    (list name address (and mac (string-downcase mac)) connection
          (network-location-hosts location))))

(define (network-location-hosts-dnsmasq-configuration config)
  "Return the entry of NetworkManager's @code{dnsmasq-configuration-files} that
makes dnsmasq serve the hosts file of CONFIG."
  `("50-network-location-hosts.conf"
    ,(plain-file "50-network-location-hosts.conf"
                 (string-append
                  "addn-hosts="
                  (network-location-hosts-configuration-hosts-file config)
                  "\n"))))

(define %dnsmasq-pid-file "/var/run/NetworkManager/dnsmasq.pid")

(define (network-location-dispatcher config)
  (program-file
   "network-location-hosts"
   #~(begin
       (use-modules (ice-9 match)
                    (ice-9 popen)
                    (ice-9 rdelim)
                    (ice-9 textual-ports)
                    (srfi srfi-1))

       (define ip #$(file-append iproute "/sbin/ip"))

       (define (command-lines . command)
         (let* ((port (apply open-pipe* OPEN_READ command))
                (text (get-string-all port)))
           (close-pipe port)
           (filter (negate string-null?) (string-split text #\newline))))

       ;; (ADDRESS . DEVICE) of the IPv4 default route, or #f.
       (define (default-gateway)
         (any (lambda (line)
                (match (string-tokenize line)
                  (("default" "via" address "dev" device . _)
                   (cons address device))
                  (_ #f)))
              (command-lines ip "-4" "route" "show" "default")))

       (define (neighbour-mac address device)
         ;; Make sure the neighbour table has the gateway right after the
         ;; link came up.
         (system* #$(file-append iputils "/bin/ping")
                  "-c" "1" "-W" "1" "-I" device address)
         (any (lambda (line)
                (match (member "lladdr" (string-tokenize line))
                  (("lladdr" mac . _) (string-downcase mac))
                  (_ #f)))
              (command-lines ip "neigh" "show" address "dev" device)))

       (define active-connections
         (delay (command-lines #$(file-append network-manager "/bin/nmcli")
                               "-t" "-f" "NAME" "connection" "show"
                               "--active")))

       (define gateway (delay (default-gateway)))

       (define (matches? address mac connection)
         (and (or (not address)
                  (match (force gateway)
                    ((gateway-address . device)
                     (and (string=? address gateway-address)
                          (or (not mac)
                              (equal? mac
                                      (neighbour-mac gateway-address
                                                     device)))))
                    (#f #f)))
              (or (not connection)
                  (member connection (force active-connections)))))

       (define (current-location)
         (find (match-lambda
                 ((name address mac connection hosts)
                  (matches? address mac connection)))
               '#$(map location->sexp
                       (network-location-hosts-configuration-locations
                        config))))

       (define (write-hosts file hosts)
         (let ((temporary (string-append file ".new")))
           (call-with-output-file temporary
             (lambda (port)
               (for-each (match-lambda
                           ((name . address)
                            (format port "~a ~a~%" address name)))
                         hosts)))
           (chmod temporary #o644)
           (rename-file temporary file)))

       (define (reload-dnsmasq)
         (let ((pid (false-if-exception
                     (call-with-input-file #$%dnsmasq-pid-file read))))
           (when (integer? pid)
             (false-if-exception (kill pid SIGHUP)))))

       ;; NetworkManager passes the interface first, the action second.
       (match (command-line)
         ((_ _ (or "up" "down" "vpn-up" "vpn-down" "dhcp4-change"
                   "connectivity-change") . _)
          (let ((file #$(network-location-hosts-configuration-hosts-file
                         config))
                (location (current-location)))
            (format #t "network-location-hosts: ~a~%"
                    (match location
                      ((name . _) name)
                      (#f "no location matches")))
            (write-hosts file (match location
                                ((_ _ _ _ hosts) hosts)
                                (#f '())))
            (reload-dnsmasq)
            (system* #$(file-append glibc "/sbin/nscd")
                     "--invalidate" "hosts")))
         (_ #t)))))

(define (network-location-hosts-activation config)
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p
       (dirname #$(network-location-hosts-configuration-hosts-file config)))
      ;; dnsmasq reads its configuration directory only at startup, so a
      ;; running one would never learn about the addn-hosts file.  Stop it;
      ;; NetworkManager starts it again with the current directory.
      (let ((pid (false-if-exception
                  (call-with-input-file #$%dnsmasq-pid-file read))))
        (when (integer? pid)
          (false-if-exception (kill pid SIGTERM))
          (sleep 2)))
      ;; Resolve right away rather than at the next network event, so that a
      ;; reconfigure takes effect at once.  At boot, before NetworkManager
      ;; is up, this matches no location and leaves an empty file, which
      ;; also keeps dnsmasq from complaining about a missing addn-hosts file.
      (system* #$(network-location-dispatcher config) "activation" "up")))

;; NetworkManager runs every regular, root-owned, non-group-writable
;; executable in this directory, symlinks included, so a store entry qualifies.
(define (network-location-hosts-special-files config)
  `(("/etc/NetworkManager/dispatcher.d/30-network-location-hosts"
     ,(network-location-dispatcher config))))

(define network-location-hosts-service-type
  (service-type
   (name 'network-location-hosts)
   (extensions
    (list (service-extension special-files-service-type
                             network-location-hosts-special-files)
          (service-extension activation-service-type
                             network-location-hosts-activation)))
   (description "Resolve host names to addresses that depend on the network
the machine is on, through NetworkManager's dnsmasq.")))
