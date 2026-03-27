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

(define-module (johnlepikhin packages xray-core)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages compression))

(define-public xray-core
  (package
    (name "xray-core")
    (version "26.3.27")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/XTLS/Xray-core/releases/download/v"
             version "/Xray-linux-64.zip"))
       (sha256
        (base32 "1bn8yw47ciihk6zfc3qykl8b5x1cjzaclpp3drvrfkbl6zwrmk93"))))
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
                      (share (string-append out "/share/xray"))
                      (tmpdir "xray-extract"))
                 (mkdir-p bin)
                 (mkdir-p share)
                 (mkdir-p tmpdir)
                 (invoke unzip source "-d" tmpdir)
                 (install-file (string-append tmpdir "/xray") bin)
                 (chmod (string-append bin "/xray") #o755)
                 (for-each
                  (lambda (f)
                    (when (file-exists? (string-append tmpdir "/" f))
                      (install-file (string-append tmpdir "/" f) share)))
                  '("geoip.dat" "geosite.dat"))
                 (delete-file-recursively tmpdir)))))
    (native-inputs
     (list unzip))
    (home-page "https://github.com/XTLS/Xray-core")
    (synopsis "Network proxy supporting VLESS, VMess, Trojan, and Shadowsocks")
    (description "Xray-core is a proxy platform for bypassing network
restrictions.  It supports multiple protocols including @code{VLESS},
@code{VMess}, @code{Trojan}, and @code{Shadowsocks} with advanced traffic
masking via @code{XTLS-Vision} and @code{Reality}.")
    (license license:mpl2.0)))
