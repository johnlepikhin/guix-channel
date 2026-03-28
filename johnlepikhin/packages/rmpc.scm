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

(define-module (johnlepikhin packages rmpc)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages base)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module ((guix licenses) #:prefix license:))

(define-public rmpc
  (package
    (name "rmpc")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/mierak/rmpc/releases/download/v"
             version "/rmpc-v" version "-x86_64-unknown-linux-gnu.tar.gz"))
       (sha256
        (base32 "017n3xn5mlcwfpymarf7ynw3jrbwgf2fxd5b0w8ydbzhgivlngdh"))))
    (supported-systems '("x86_64-linux"))
    (build-system gnu-build-system)
    (arguments
     `(#:strip-binaries? #f
       #:validate-runpath? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (delete 'check)
         ;; The tarball has no root directory; unpack enters the first
         ;; subdirectory (completions/), so we step back up.
         (add-after 'unpack 'enter-source
           (lambda _
             (chdir "..")))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (man1 (string-append out "/share/man/man1"))
                    (bash-comp (string-append out "/share/bash-completion/completions"))
                    (zsh-comp (string-append out "/share/zsh/site-functions"))
                    (fish-comp (string-append out "/share/fish/vendor_completions.d"))
                    (patchelf (string-append
                               (assoc-ref inputs "patchelf")
                               "/bin/patchelf"))
                    (libc (assoc-ref inputs "libc"))
                    (ld-so (string-append libc "/lib/ld-linux-x86-64.so.2"))
                    (gcc-lib (assoc-ref inputs "gcc"))
                    (rpath (string-join
                            (list (string-append libc "/lib")
                                  (if gcc-lib (string-append gcc-lib "/lib") ""))
                            ":")))
               (install-file "rmpc" bin)
               (invoke patchelf "--set-interpreter" ld-so
                       "--set-rpath" rpath
                       (string-append bin "/rmpc"))
               (install-file "man/rmpc.1" man1)
               (install-file "completions/rmpc.bash" bash-comp)
               (rename-file (string-append bash-comp "/rmpc.bash")
                            (string-append bash-comp "/rmpc"))
               (install-file "completions/_rmpc" zsh-comp)
               (install-file "completions/rmpc.fish" fish-comp)))))))
    (native-inputs
     (list patchelf))
    (inputs
     (list glibc
           `(,gcc "lib")))
    (home-page "https://mierak.github.io/rmpc/")
    (synopsis "Configurable TUI client for MPD")
    (description "rmpc is a terminal-based Music Player Daemon client heavily
inspired by @command{ncmpcpp} and @command{ranger}/@command{lf} file managers.
It has support for synchronized lyrics, and displaying album cover art with
various terminal image protocols.")
    (license license:bsd-3)))
