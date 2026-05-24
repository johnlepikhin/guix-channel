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

;; Shared helpers for Rust packages in this channel.

(define-module (johnlepikhin build rust)
  #:use-module (guix gexp)
  #:export (%rust-cc-symlink-phase))

;; A `modify-phases` clause that drops a `cc -> gcc` symlink into a
;; build-local bin directory and exports PATH/CC/HOST_CC.  Cargo's
;; build.rs scripts (and the `cc` crate) invoke `cc` by name without a
;; toolchain prefix; Guix's gcc-toolchain only exposes `gcc`, so
;; without this shim every cargo-build-system package whose deps
;; compile any C breaks at link time.
;;
;; Use a build-local `.cc-bin` directory rather than `/tmp/bin`: under
;; concurrent builds (`guix build -M N`) two derivations would race
;; over the global path.
;;
;; Splice into a `modify-phases` form via `#$%rust-cc-symlink-phase`:
;;
;;   #:phases
;;   #~(modify-phases %standard-phases
;;       #$%rust-cc-symlink-phase
;;       (add-after 'install ...))
;;
;; The phase expects a `gcc-toolchain` input to be present.
(define %rust-cc-symlink-phase
  #~(add-before 'configure 'create-cc-symlink
      (lambda* (#:key inputs #:allow-other-keys)
        (let* ((gcc (assoc-ref inputs "gcc-toolchain"))
               (bin-dir (string-append (getcwd) "/.cc-bin")))
          (mkdir-p bin-dir)
          (symlink (string-append gcc "/bin/gcc")
                   (string-append bin-dir "/cc"))
          (setenv "PATH" (string-append bin-dir ":" (getenv "PATH")))
          (setenv "CC" (string-append gcc "/bin/gcc"))
          (setenv "HOST_CC" (string-append gcc "/bin/gcc"))))))
