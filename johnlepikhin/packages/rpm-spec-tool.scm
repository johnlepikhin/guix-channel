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

(define-module (johnlepikhin packages rpm-spec-tool)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages commencement)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (johnlepikhin packages rust-binary)
  #:use-module (johnlepikhin packages rust-crates))

(define-public rpm-spec-tool
  (package
    (name "rpm-spec-tool")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri name version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1jawpnmyhalr83mblz3s0srs4hj7sg0qww3c2n8lcd15j7sswp4x"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:rust rust-binary-1.88
      #:install-source? #f
      ;; Integration tests reference fixtures outside the crate tarball.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'create-cc-symlink
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((gcc (assoc-ref inputs "gcc-toolchain"))
                     (bin-dir (string-append (getcwd) "/.cc-bin")))
                (mkdir-p bin-dir)
                (symlink (string-append gcc "/bin/gcc")
                         (string-append bin-dir "/cc"))
                (setenv "PATH" (string-append bin-dir ":" (getenv "PATH")))
                (setenv "CC" (string-append gcc "/bin/gcc"))
                (setenv "HOST_CC" (string-append gcc "/bin/gcc"))))))))
    (native-inputs (list gcc-toolchain))
    (inputs (cargo-inputs 'rpm-spec-tool
                          #:module '(johnlepikhin packages rust-crates)))
    (home-page "https://github.com/johnlepikhin/rpm-spec-tool")
    (synopsis "Pretty-printer and static analyzer for RPM .spec files")
    (description
     "@command{rpm-spec-tool} is a command-line pretty-printer and static
analyzer for RPM @file{.spec} files.  It reformats spec files according to
configurable style rules and reports lint-style diagnostics about common
authoring mistakes, helping packagers keep spec files consistent and
correct.  The tool ships with built-in distribution profiles (generic,
RHEL, Fedora, SUSE, ALT Linux) and optionally integrates with
@command{shellcheck} to lint embedded shell sections.")
    (license (list license:expat license:asl2.0))))
