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

(define-module (johnlepikhin packages antigravity-cli)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (nonguix licenses))

;; Google Antigravity CLI (`agy'), the terminal agent of Antigravity.
;;
;; Distribution notes.  Releases live under
;; <https://storage.googleapis.com/antigravity-public/antigravity-cli/>:
;; `latest' holds the newest version, and `<VERSION>/manifest.json' the URL
;; and SHA-512 of each platform's tarball.  The URLs carry a build ID next
;; to the version, which is why it is kept here:
;;   v=$(curl -s .../antigravity-cli/latest)
;;   curl -s .../antigravity-cli/$v/manifest.json
;;
;; Each tarball holds a single `antigravity' Go binary.  We take the
;; `linux-x64-musl' build: it is statically linked, so it needs no
;; patchelf, unlike the glibc build, which asks for /lib64/ld-linux.  Being
;; Go, it reads the CA certificates from /etc/ssl/certs.  The binary
;; updates itself in place when its AutoUpdate setting allows; in the store
;; that cannot happen, and Guix decides the version.
(define-public antigravity-cli
  (let ((build-id "4602374149505024"))
    (package
      (name "antigravity-cli")
      (version "1.5.8")
      (source
       (origin
         (method url-fetch)
         (uri (string-append
               "https://storage.googleapis.com/antigravity-public/"
               "antigravity-cli/" version "-" build-id
               "/linux-x64-musl/cli_linux_x64_musl.tar.gz"))
         (sha256
          (base32 "1i3lna5i1x5xk6d4y66zdrlbvcvnvp1daxgyya00hlbbrdwc2wfx"))))
      (build-system trivial-build-system)
      (arguments
       (list #:modules '((guix build utils))
             #:builder
             #~(begin
                 (use-modules (guix build utils))
                 (let ((bin (string-append #$output "/bin")))
                   (setenv "PATH" (string-append #$(this-package-input "gzip")
                                                 "/bin"))
                   (mkdir-p bin)
                   (invoke #$(file-append (this-package-input "tar")
                                          "/bin/tar")
                           "-xzf" #$source "-C" bin "antigravity")
                   (rename-file (string-append bin "/antigravity")
                                (string-append bin "/agy"))))))
      (inputs (list gzip tar))
      (supported-systems '("x86_64-linux"))
      (home-page "https://antigravity.google/product/antigravity-cli")
      (synopsis "Terminal agent of Google Antigravity")
      (description
       "Antigravity CLI, run as @command{agy}, brings the agent harness of
Google Antigravity to the terminal: it reads and edits the code of the
current project, runs commands and tools with the user's permission, and
talks to MCP servers.  It works interactively or, with @option{--print}, as
a single non-interactive prompt.")
      (license (nonfree "https://antigravity.google/terms")))))
