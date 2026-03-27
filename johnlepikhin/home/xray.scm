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

(define-module (johnlepikhin home xray)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages linux)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (johnlepikhin packages xray-core)
  #:use-module (johnlepikhin packages tun2socks)
  #:export (home-xray-configuration
            home-xray-configuration?
            home-xray-configuration-package
            home-xray-configuration-tun2socks
            home-xray-configuration-config-file
            home-xray-configuration-server-address
            home-xray-configuration-socks-port
            home-xray-configuration-tun-name
            home-xray-configuration-tun-address
            home-xray-configuration-tun-gateway
            home-xray-service-type))

;;; Module: (johnlepikhin home xray)
;;;
;;; Guix Home service for Xray VPN proxy with tun2socks.
;;;
;;; Installs start/stop/status scripts for manual VPN control.
;;; Uses tun2socks to create a TUN interface that routes all system
;;; traffic through xray's SOCKS5 proxy.  Does not modify iptables.
;;;
;;; Example:
;;;
;;;   (service home-xray-service-type
;;;     (home-xray-configuration
;;;       (config-file (local-file "xray-config.json"))
;;;       (server-address "1.2.3.4")))

(define-record-type* <home-xray-configuration>
  home-xray-configuration make-home-xray-configuration
  home-xray-configuration?
  (package        home-xray-configuration-package
                  (default xray-core))
  (tun2socks      home-xray-configuration-tun2socks
                  (default tun2socks))
  (config-file    home-xray-configuration-config-file)
  (server-address home-xray-configuration-server-address)
  (socks-port     home-xray-configuration-socks-port
                  (default 10808))
  (tun-name       home-xray-configuration-tun-name
                  (default "tun-xray"))
  (tun-address    home-xray-configuration-tun-address
                  (default "198.18.0.1/15"))
  (tun-gateway    home-xray-configuration-tun-gateway
                  (default "198.18.0.1")))

(define (make-start-script config)
  (let ((xray-pkg    (home-xray-configuration-package config))
        (t2s-pkg     (home-xray-configuration-tun2socks config))
        (config-file (home-xray-configuration-config-file config))
        (server-addr (home-xray-configuration-server-address config))
        (socks-port  (number->string (home-xray-configuration-socks-port config)))
        (tun-name    (home-xray-configuration-tun-name config))
        (tun-address (home-xray-configuration-tun-address config))
        (tun-gateway (home-xray-configuration-tun-gateway config))
        (ip-cmd      (file-append iproute "/sbin/ip"))
        (ss-cmd      (file-append iproute "/sbin/ss")))
    (computed-file
     "xray-start"
     #~(begin
         (with-output-to-file #$output
           (lambda ()
             (display
              (string-append
               "#!/bin/sh\n"
               "set -e\n\n"

               "RUNTIME_DIR=\"${XDG_RUNTIME_DIR:-/run/user/$(id -u)}\"\n"
               "XRAY_PID=\"$RUNTIME_DIR/xray.pid\"\n"
               "T2S_PID=\"$RUNTIME_DIR/tun2socks.pid\"\n"
               "TUN=\"" #$tun-name "\"\n"
               "SERVER=\"" #$server-addr "\"\n\n"

               ;; Check if already running
               "if [ -f \"$XRAY_PID\" ] && kill -0 \"$(cat \"$XRAY_PID\")\" 2>/dev/null; then\n"
               "  echo 'xray-proxy is already running'\n"
               "  exit 1\n"
               "fi\n\n"

               ;; Start xray
               "LOG_DIR=\"${XDG_STATE_HOME:-$HOME/.local/state}/log\"\n"
               "mkdir -p \"$LOG_DIR\"\n\n"
               "echo 'Starting xray...'\n"
               "XRAY_LOCATION_ASSET=" #$(file-append xray-pkg "/share/xray")
               " " #$(file-append xray-pkg "/bin/xray")
               " run -c " #$config-file
               " >>\"$LOG_DIR/xray.log\" 2>&1 &\n"
               "echo $! > \"$XRAY_PID\"\n\n"

               ;; Wait for SOCKS5 port
               "echo 'Waiting for SOCKS5 proxy on port " #$socks-port "...'\n"
               "for i in $(seq 1 30); do\n"
               "  if " #$ss-cmd " -tln sport = :" #$socks-port " | grep -q LISTEN; then\n"
               "    break\n"
               "  fi\n"
               "  if [ \"$i\" = 30 ]; then\n"
               "    echo 'ERROR: xray failed to start'\n"
               "    kill \"$(cat \"$XRAY_PID\")\" 2>/dev/null || true\n"
               "    rm -f \"$XRAY_PID\"\n"
               "    exit 1\n"
               "  fi\n"
               "  sleep 0.5\n"
               "done\n"
               "echo 'xray is ready'\n\n"

               ;; Detect current default gateway
               "ORIG_GW=$(" #$ip-cmd " route show default | head -1 | awk '{print $3}')\n"
               "ORIG_DEV=$(" #$ip-cmd " route show default | head -1 | awk '{print $5}')\n"
               "if [ -z \"$ORIG_GW\" ] || [ -z \"$ORIG_DEV\" ]; then\n"
               "  echo 'ERROR: cannot detect default gateway'\n"
               "  kill \"$(cat \"$XRAY_PID\")\" 2>/dev/null || true\n"
               "  rm -f \"$XRAY_PID\"\n"
               "  exit 1\n"
               "fi\n"
               "echo \"Original gateway: $ORIG_GW via $ORIG_DEV\"\n\n"

               ;; Create TUN interface
               "echo 'Creating TUN interface...'\n"
               "sudo " #$ip-cmd " tuntap add mode tun dev \"$TUN\"\n"
               "sudo " #$ip-cmd " addr add " #$tun-address " dev \"$TUN\"\n"
               "sudo " #$ip-cmd " link set dev \"$TUN\" up\n\n"

               ;; Start tun2socks
               "echo 'Starting tun2socks...'\n"
               "sudo " #$(file-append t2s-pkg "/bin/tun2socks")
               " -device \"$TUN\""
               " -proxy socks5://127.0.0.1:" #$socks-port
               " >>\"$LOG_DIR/tun2socks.log\" 2>&1 &\n"
               "echo $! > \"$T2S_PID\"\n"
               "sleep 1\n\n"

               ;; Add bypass route for xray server
               "echo 'Configuring routes...'\n"
               "sudo " #$ip-cmd " route add \"$SERVER\"/32 via $ORIG_GW dev $ORIG_DEV 2>/dev/null || true\n\n"

               ;; Add default route through TUN with high priority
               "sudo " #$ip-cmd " route add default via " #$tun-gateway " dev \"$TUN\" metric 1\n\n"

               "echo 'xray-proxy is active. All traffic routed through VPN.'\n"))))
         (chmod #$output #o755)))))

(define (make-stop-script config)
  (let ((tun-name (home-xray-configuration-tun-name config))
        (ip-cmd  (file-append iproute "/sbin/ip")))
    (computed-file
     "xray-stop"
     #~(begin
         (with-output-to-file #$output
           (lambda ()
             (display
              (string-append
               "#!/bin/sh\n\n"

               "RUNTIME_DIR=\"${XDG_RUNTIME_DIR:-/run/user/$(id -u)}\"\n"
               "XRAY_PID=\"$RUNTIME_DIR/xray.pid\"\n"
               "T2S_PID=\"$RUNTIME_DIR/tun2socks.pid\"\n"
               "TUN=\"" #$tun-name "\"\n\n"

               ;; Kill tun2socks
               "if [ -f \"$T2S_PID\" ]; then\n"
               "  echo 'Stopping tun2socks...'\n"
               "  sudo kill \"$(cat \"$T2S_PID\")\" 2>/dev/null || true\n"
               "  rm -f \"$T2S_PID\"\n"
               "fi\n\n"

               ;; Kill xray
               "if [ -f \"$XRAY_PID\" ]; then\n"
               "  echo 'Stopping xray...'\n"
               "  kill \"$(cat \"$XRAY_PID\")\" 2>/dev/null || true\n"
               "  rm -f \"$XRAY_PID\"\n"
               "fi\n\n"

               ;; Delete TUN — all routes through it are removed by kernel
               "if " #$ip-cmd " link show \"$TUN\" >/dev/null 2>&1; then\n"
               "  echo 'Removing TUN interface...'\n"
               "  sudo " #$ip-cmd " link delete \"$TUN\"\n"
               "fi\n\n"

               "echo 'xray-proxy stopped.'\n"))))
         (chmod #$output #o755)))))

(define (make-status-script config)
  (let ((tun-name   (home-xray-configuration-tun-name config))
        (socks-port (number->string (home-xray-configuration-socks-port config)))
        (ip-cmd     (file-append iproute "/sbin/ip"))
        (ss-cmd     (file-append iproute "/sbin/ss")))
    (computed-file
     "xray-status"
     #~(begin
         (with-output-to-file #$output
           (lambda ()
             (display
              (string-append
               "#!/bin/sh\n\n"

               "RUNTIME_DIR=\"${XDG_RUNTIME_DIR:-/run/user/$(id -u)}\"\n"
               "XRAY_PID=\"$RUNTIME_DIR/xray.pid\"\n"
               "T2S_PID=\"$RUNTIME_DIR/tun2socks.pid\"\n"
               "TUN=\"" #$tun-name "\"\n\n"

               "RUNNING=true\n\n"

               ;; Check xray
               "if [ -f \"$XRAY_PID\" ] && kill -0 \"$(cat \"$XRAY_PID\")\" 2>/dev/null; then\n"
               "  echo \"xray: running (PID $(cat \"$XRAY_PID\"))\"\n"
               "else\n"
               "  echo 'xray: not running'\n"
               "  RUNNING=false\n"
               "fi\n\n"

               ;; Check tun2socks
               "if [ -f \"$T2S_PID\" ] && sudo kill -0 \"$(cat \"$T2S_PID\")\" 2>/dev/null; then\n"
               "  echo \"tun2socks: running (PID $(cat \"$T2S_PID\"))\"\n"
               "else\n"
               "  echo 'tun2socks: not running'\n"
               "  RUNNING=false\n"
               "fi\n\n"

               ;; Check TUN interface
               "if " #$ip-cmd " link show \"$TUN\" >/dev/null 2>&1; then\n"
               "  echo \"interface: $TUN is up\"\n"
               "else\n"
               "  echo \"interface: $TUN does not exist\"\n"
               "  RUNNING=false\n"
               "fi\n\n"

               ;; Check SOCKS5 port
               "if " #$ss-cmd " -tln sport = :" #$socks-port " | grep -q LISTEN; then\n"
               "  echo 'socks5: listening on port " #$socks-port "'\n"
               "else\n"
               "  echo 'socks5: not listening'\n"
               "  RUNNING=false\n"
               "fi\n\n"

               "if $RUNNING; then\n"
               "  echo '\\nStatus: ACTIVE'\n"
               "else\n"
               "  echo '\\nStatus: INACTIVE'\n"
               "fi\n"))))
         (chmod #$output #o755)))))

(define (add-xray-files config)
  `((".local/bin/xray-start"  ,(make-start-script config))
    (".local/bin/xray-stop"   ,(make-stop-script config))
    (".local/bin/xray-status" ,(make-status-script config))))

(define (add-xray-config-files config)
  `(("xray/config.json" ,(home-xray-configuration-config-file config))))

(define (add-xray-packages config)
  (list (home-xray-configuration-package config)
        (home-xray-configuration-tun2socks config)))

(define home-xray-service-type
  (service-type
   (name 'home-xray)
   (extensions
    (list
     (service-extension home-files-service-type add-xray-files)
     (service-extension home-xdg-configuration-files-service-type
                        add-xray-config-files)
     (service-extension home-profile-service-type add-xray-packages)))
   (description "Install Xray VPN proxy with tun2socks start/stop scripts.")))
