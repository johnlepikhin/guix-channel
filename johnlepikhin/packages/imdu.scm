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

(define-module (johnlepikhin packages imdu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix build-system cargo)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (johnlepikhin build rust)
  #:use-module (johnlepikhin packages rust-binary)
  #:use-module (johnlepikhin packages rust-crates))

(define-public imdu
  (package
    (name "imdu")
    (version "0.2.0")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/johnlepikhin/imdu.git")
                   (commit (string-append "v" version))))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "0l3cvvbyfv0c9c7i2skyglc7sm7wp7n353c125nxgjlbg4nwxa3h"))))
    (build-system cargo-build-system)
    (arguments
     (list
      ;; Cargo.toml declares edition 2024 and rust-version 1.88 (ratatui and
      ;; the let-chains in the walker both need it).
      #:rust rust-binary-1.88
      #:install-source? #f
      #:phases
      #~(modify-phases %standard-phases
          #$%rust-cc-symlink-phase
          (add-before 'configure 'use-system-zstd
            (lambda _
              ;; zstd-sys vendors a copy of the C library and builds it by
              ;; default; link against the Guix one instead.
              (setenv "ZSTD_SYS_USE_PKG_CONFIG" "1")))
          (add-after 'install 'install-man-page
            (lambda _
              (install-file "man/imdu.1"
                            (string-append #$output "/share/man/man1")))))))
    (native-inputs (list gcc-toolchain pkg-config))
    (inputs (append (list `(,zstd "lib"))
                    (cargo-inputs 'imdu
                                  #:module '(johnlepikhin packages rust-crates))))
    (home-page "https://github.com/johnlepikhin/imdu")
    (synopsis "Disk usage reporter with live ranking and honest incompleteness")
    (description "imdu reports directory sizes like @command{du}, and answers
three questions @command{du} cannot.  It shows the ranking of the largest
directories while the scan is still running, marks any figure that is a lower
bound because something could not be read, and explains why @command{df}
disagrees with it, typically because of deleted-but-still-open files.  Runs can
be bounded by a time budget, and snapshots can be saved and diffed.  Linux
only: it relies on @file{/proc/self/mountinfo}, @code{statx} and
@code{st_blocks} accounting.")
    (license (list license:expat license:asl2.0))))
