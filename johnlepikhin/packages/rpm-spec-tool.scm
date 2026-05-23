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
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix packages))

(define-public rpm-spec-tool
  (package
    (name "rpm-spec-tool")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/johnlepikhin/rpm-spec-tool/releases/download/v"
             version "/rpm-spec-tool-" version "-linux-x86_64.tar.gz"))
       (sha256
        (base32 "0wxp0bm992b0k8qq8x2h9jfxv4kscy85j2mx3ss0y393s9zh8r9v"))))
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
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (patchelf (string-append
                               (assoc-ref inputs "patchelf")
                               "/bin/patchelf"))
                    (libc (assoc-ref inputs "libc"))
                    (ld-so (string-append libc "/lib/ld-linux-x86-64.so.2"))
                    (gcc-lib (assoc-ref inputs "gcc"))
                    (xz (assoc-ref inputs "xz"))
                    (rpath (string-join
                            (list (string-append libc "/lib")
                                  (string-append gcc-lib "/lib")
                                  (string-append xz "/lib"))
                            ":")))
               (for-each (lambda (binary)
                           (install-file binary bin)
                           (invoke patchelf
                                   "--set-interpreter" ld-so
                                   "--set-rpath" rpath
                                   (string-append bin "/" binary)))
                         '("rpm-spec-tool" "rpm-spec-lsp"))))))))
    (native-inputs
     (list patchelf))
    (inputs
     (list glibc
           xz
           `(,gcc "lib")))
    (home-page "https://github.com/johnlepikhin/rpm-spec-tool")
    (synopsis "Pretty-printer, static analyzer and LSP server for RPM .spec files")
    (description
     "@command{rpm-spec-tool} is a command-line pretty-printer and static
analyzer for RPM @file{.spec} files.  It reformats spec files according to
configurable style rules and reports lint-style diagnostics about common
authoring mistakes, helping packagers keep spec files consistent and
correct.  The tool ships with built-in distribution profiles (generic,
RHEL, Fedora, SUSE, ALT Linux) and optionally integrates with
@command{shellcheck} to lint embedded shell sections.  The package also
provides @command{rpm-spec-lsp}, a Language Server Protocol implementation
that powers editor integrations (diagnostics, formatting, completion) for
@file{.spec} files.")
    (license (list license:expat license:asl2.0))))
