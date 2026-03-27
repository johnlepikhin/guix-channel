;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Evgenii Lepikhin <johnlepikhin@gmail.com>
;;;
;;; This file is part of a Guix channel.
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define-module (johnlepikhin packages tun2socks)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages compression))

(define-public tun2socks
  (package
    (name "tun2socks")
    (version "2.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/xjasonlyu/tun2socks/releases/download/v"
             version "/tun2socks-linux-amd64.zip"))
       (sha256
        (base32 "0xvyhs684xsv8srm521j10q7jrjcsh98c59db0mwp3l9ra8rhk9c"))))
    (supported-systems '("x86_64-linux"))
    (build-system trivial-build-system)
    (arguments
     (list #:modules '((guix build utils))
           #:builder
           #~(begin
               (use-modules (guix build utils))
               (let* ((source #$source)
                      (unzip #$(file-append unzip "/bin/unzip"))
                      (out #$output)
                      (bin (string-append out "/bin"))
                      (tmpdir "tun2socks-extract"))
                 (mkdir-p bin)
                 (mkdir-p tmpdir)
                 (invoke unzip source "-d" tmpdir)
                 (copy-file (string-append tmpdir "/tun2socks-linux-amd64")
                            (string-append bin "/tun2socks"))
                 (chmod (string-append bin "/tun2socks") #o755)
                 (delete-file-recursively tmpdir)))))
    (native-inputs
     (list unzip))
    (home-page "https://github.com/xjasonlyu/tun2socks")
    (synopsis "Tunnel TCP/UDP connections through a SOCKS5 proxy via TUN device")
    (description "@code{tun2socks} creates a virtual @acronym{TUN} network
interface and forwards all TCP and UDP traffic through a @code{SOCKS5} or
HTTP proxy.  This enables system-wide proxying without modifying
@code{iptables} rules.")
    (license license:gpl3)))
