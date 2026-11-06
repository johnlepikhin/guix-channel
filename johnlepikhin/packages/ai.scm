;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024, 2026 Evgenii Lepikhin <johnlepikhin@gmail.com>
;;;
;;; This file is part of GNU Guix.
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

(define-module (johnlepikhin packages ai)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages virtualization)
  #:use-module (nonguix licenses))

;; Anthropic Claude Code CLI.
;;
;; Upstream switched the delivery in late 2025: the @anthropic-ai/claude-code
;; npm package became an ~18 KB wrapper whose postinstall (install.cjs)
;; resolves a platform-specific optionalDependency to find the real binary.
;; The native artefact ships in a separate npm package per OS/arch -- for
;; linux-x64 that's `@anthropic-ai/claude-code-linux-x64`, containing a single
;; ~230 MB self-contained ELF binary `package/claude` with NEEDED limited to
;; glibc-level libraries plus `/lib64/ld-linux-x86-64.so.2` as the interpreter.
;;
;; We bypass the npm wrapper entirely and pull the linux-x64 tarball directly:
;; trivial-build-system, patchelf the interpreter + RUNPATH to point at Guix's
;; glibc, and place the binary as `bin/.claude-real` with a shell-wrapper at
;; `bin/claude` that puts ripgrep / git / coreutils on PATH so the embedded
;; Grep tool and shell-out for git find their dependencies without dragging
;; in the user's full profile.
(define-public claude-code
  (package
    (name "claude-code")
    (version "2.1.280")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://registry.npmjs.org/"
                           "@anthropic-ai/claude-code-linux-x64/-/"
                           "claude-code-linux-x64-" version ".tgz"))
       (sha256
        (base32 "1lxza9nxmzhxazgfhsmg6ds563fsymjfpwvfaffzf0p300qmg59x"))))
    (build-system trivial-build-system)
    (arguments
     (list #:modules '((guix build utils))
           #:builder
           #~(begin
               (use-modules (guix build utils))
               (let* ((source #$source)
                      (tar (string-append #$(file-append tar "/bin/tar")))
                      (patchelf (string-append
                                 #$(file-append patchelf "/bin/patchelf")))
                      (bash (string-append
                             #$(file-append bash-minimal "/bin/bash")))
                      (glibc-lib (string-append #$(file-append glibc "/lib")))
                      (out #$output)
                      (bin (string-append out "/bin"))
                      (tmpdir "claude-extract"))

                 (mkdir-p bin)
                 (mkdir-p tmpdir)

                 ;; gzip is required by GNU tar for `-z`.
                 (setenv "PATH"
                         (string-append #$(file-append gzip "/bin")))
                 (invoke tar "-xzf" source "-C" tmpdir)

                 ;; The tarball lays out as `package/claude` (plus the
                 ;; package.json + LICENSE.md metadata that we drop on the
                 ;; floor -- the binary is self-contained).
                 (copy-file (string-append tmpdir "/package/claude")
                            (string-append bin "/.claude-real"))
                 (chmod (string-append bin "/.claude-real") #o755)

                 ;; The upstream ELF carries `/lib64/ld-linux-x86-64.so.2` as
                 ;; its PT_INTERP, which doesn't exist on Guix.  Repoint at
                 ;; our glibc's ld-linux; the loader then finds its sibling
                 ;; libc/librt/libpthread/libdl/libm in its own dir via the
                 ;; standard "loader's directory" lookup, so no DT_RUNPATH
                 ;; is needed.  Importantly, `patchelf --set-rpath` on this
                 ;; ~230 MB Bun-compiled binary corrupts segment offsets and
                 ;; the result segfaults before main() -- the embedded JS
                 ;; snapshot is mmap'd at fixed offsets that don't survive
                 ;; section rewrites.  set-interpreter is a pure metadata
                 ;; tweak in PT_INTERP and is safe.
                 (invoke patchelf "--set-interpreter"
                         (string-append glibc-lib "/ld-linux-x86-64.so.2")
                         (string-append bin "/.claude-real"))

                 ;; Thin shell-wrapper.  Claude shells out to ripgrep for its
                 ;; built-in Grep tool, to git for version-control ops, and
                 ;; to plain coreutils (find/ls/cat).  Pinning them by store
                 ;; path keeps the agent reproducible even if the user's
                 ;; PATH is empty -- the upstream-prepended PATH still wins
                 ;; over the user's, but their values are appended via $PATH.
                 (call-with-output-file (string-append bin "/claude")
                   (lambda (port)
                     (format port "#!~a~%~
                                  export PATH=~a:~a:~a:$PATH~%~
                                  exec ~a \"$@\"~%"
                             bash
                             #$(file-append ripgrep "/bin")
                             #$(file-append git-minimal "/bin")
                             #$(file-append coreutils "/bin")
                             (string-append bin "/.claude-real"))))
                 (chmod (string-append bin "/claude") #o755)

                 (delete-file-recursively tmpdir)))))
    (native-inputs
     (list tar gzip patchelf))
    (inputs
     (list glibc bash-minimal coreutils ripgrep git-minimal))
    (home-page "https://github.com/anthropics/claude-code")
    (synopsis "AI-powered coding assistant for the terminal")
    (description
     "Claude Code is Anthropic's agentic coding tool that runs in your
terminal.  It understands the codebase, edits files, runs shell commands and
handles git workflows from a natural-language interface.  This package wires
in @code{ripgrep} (for the built-in Grep tool), @code{git} (for repository
operations) and @code{coreutils} via a shell wrapper so the agent works
without polluting the user's PATH or relying on the npm wrapper's optional
dependency resolution.")
    ;; Proprietary upstream: see LICENSE.md inside the tarball.
    ;; "© Anthropic PBC. All rights reserved. Use is subject to the Legal
    ;; Agreements outlined here: https://code.claude.com/docs/en/legal-and-compliance"
    (license (nonfree
              "https://code.claude.com/docs/en/legal-and-compliance"))))

;; URI of the static Linux build of release asset NAME.
(define (codex-release-uri version name)
  (string-append "https://github.com/openai/codex/releases/download/rust-v"
                 version "/" name "-x86_64-unknown-linux-musl.tar.gz"))

;; OpenAI Codex CLI.
;;
;; Upstream publishes fully static (musl, static-pie) builds per release, so
;; unlike claude-code no patchelf is needed: the binaries are unpacked as-is
;; into `libexec/codex'.  Building from source is impractical given the size
;; of the cargo dependency graph.
;;
;; Code Mode runs JavaScript in a separate `codex-code-mode-host' process,
;; which codex looks up next to its own executable.  It is shipped as a
;; separate release asset; keeping both in `libexec/codex' satisfies that
;; lookup without exposing the host on the profile's PATH.
;;
;; The Linux sandbox looks up `bwrap' on PATH (falling back to a bundled
;; `codex-resources/bwrap' next to the executable, which we do not ship) and
;; shells out to `rg' for file search and deny-glob expansion.  The wrapper
;; pins bubblewrap, ripgrep, git and coreutils by store path for that.
(define-public codex
  (package
    (name "codex")
    (version "0.167.0")
    (source
     (origin
       (method url-fetch)
       (uri (codex-release-uri version "codex"))
       (sha256
        (base32 "06i5vbmjd7sbld0pxgaqcl8afz6ypiz4qig41vlp9cym0rhq6r4x"))))
    (build-system trivial-build-system)
    (arguments
     (list #:modules '((guix build utils))
           #:builder
           #~(begin
               (use-modules (guix build utils))
               (let* ((tar #$(file-append tar "/bin/tar"))
                      (bash #$(file-append bash-minimal "/bin/bash"))
                      (bin (string-append #$output "/bin"))
                      (libexec (string-append #$output "/libexec/codex"))
                      (real (string-append libexec "/codex"))
                      (tmpdir "codex-extract"))

                 (define (install-binary archive name)
                   ;; Each tarball holds a single file named after the target.
                   (let ((target (string-append libexec "/" name)))
                     (invoke tar "-xzf" archive "-C" tmpdir)
                     (copy-file (string-append tmpdir "/" name
                                               "-x86_64-unknown-linux-musl")
                                target)
                     (chmod target #o755)))

                 (mkdir-p bin)
                 (mkdir-p libexec)
                 (mkdir-p tmpdir)

                 ;; gzip is required by GNU tar for `-z'.
                 (setenv "PATH" #$(file-append gzip "/bin"))
                 (install-binary #$source "codex")
                 (install-binary
                  #$(origin
                      (method url-fetch)
                      (uri (codex-release-uri version "codex-code-mode-host"))
                      (sha256
                       (base32
                        "1m3xy1n9lpy4wb0i4zx2yp0k3zwfb154jzljfg0b4hs6srfls1xm")))
                  "codex-code-mode-host")

                 (call-with-output-file (string-append bin "/codex")
                   (lambda (port)
                     (format port "#!~a~%~
                                  export PATH=~a:~a:~a:~a:$PATH~%~
                                  exec ~a \"$@\"~%"
                             bash
                             #$(file-append ripgrep "/bin")
                             #$(file-append git-minimal "/bin")
                             #$(file-append bubblewrap "/bin")
                             #$(file-append coreutils "/bin")
                             real)))
                 (chmod (string-append bin "/codex") #o755)

                 (delete-file-recursively tmpdir)))))
    (native-inputs
     (list tar gzip))
    (inputs
     (list bash-minimal bubblewrap coreutils ripgrep git-minimal))
    (supported-systems '("x86_64-linux"))
    (home-page "https://github.com/openai/codex")
    (synopsis "OpenAI's coding agent for the terminal")
    (description
     "Codex CLI is OpenAI's agentic coding tool that runs locally in the
terminal.  It reads and edits the codebase and runs commands inside a
@code{bubblewrap}-based sandbox.  This package wires in @code{bubblewrap},
@code{ripgrep}, @code{git} and @code{coreutils} via a shell wrapper.")
    (license license:asl2.0)))
