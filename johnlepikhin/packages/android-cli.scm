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

(define-module (johnlepikhin packages android-cli)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (nonguix licenses))

;; Google Android CLI -- the agent-oriented command line interface for Android
;; development (project scaffolding, SDK management, emulators, `run', UI
;; layout dumps, screenshots, docs search and the Android skills for coding
;; agents).
;;
;; Distribution notes.  Google ships three artefacts under
;; <https://dl.google.com/android/cli/>:
;;
;;   * `latest/linux_x86_64/android'      -- a ~5 MB launcher stub that
;;     downloads the real CLI into $ANDROID_USER_HOME/bin and can update it
;;     in place;
;;   * `latest/linux_x86_64/android-cli'  -- the real ~85 MB binary;
;;   * `latest/debian/...'                -- an APT repository carrying the
;;     launcher stub only.
;;
;; We package the real binary directly and skip the launcher: a self-updating
;; stub is pointless when the store is read-only, and it would make the
;; installed version invisible to Guix.  `latest/' has no stable content, but
;; the same file is served from a versioned path, verified byte-identical:
;; `1.0.15985488/linux_x86_64/android-cli'.  `latest/' is kept as a fallback
;; URI in case Google prunes the versioned directory.
;;
;; There is no `guix refresh' updater for this.  New versions are found with
;;   curl https://dl.google.com/android/cli/latest/linux_x86_64/METADATA
;; which prints `version=<VERSION>'.
;;
;; The binary embeds its own JRE, which it unpacks into
;; $ANDROID_USER_HOME/cli/bundles/<hash>/jre on first run, so no external JDK
;; is required to run the CLI itself (Gradle still needs one).
(define-public android-cli
  (package
    (name "android-cli")
    (version "1.0.15985488")
    (source
     (origin
       (method url-fetch)
       (uri (list (string-append "https://dl.google.com/android/cli/"
                                 version "/linux_x86_64/android-cli")
                  (string-append "https://dl.google.com/android/cli/"
                                 "latest/linux_x86_64/android-cli")))
       (file-name (string-append "android-cli-" version))
       (sha256
        (base32 "1mj04i8fz94isg0gkj6srnll4ll9cdr9jjs8320qgxd4rnwjjgj8"))))
    (build-system trivial-build-system)
    (arguments
     (list #:modules '((guix build utils))
           #:builder
           #~(begin
               (use-modules (guix build utils))
               (let* ((patchelf #$(file-append patchelf "/bin/patchelf"))
                      (bash #$(file-append bash-minimal "/bin/bash"))
                      (glibc-lib #$(file-append glibc "/lib"))
                      (out #$output)
                      (bin (string-append out "/bin"))
                      (real (string-append bin "/.android-cli-real")))

                 (mkdir-p bin)
                 (copy-file #$source real)
                 (chmod real #o755)

                 ;; The upstream ELF carries `/lib64/ld-linux-x86-64.so.2' as
                 ;; its PT_INTERP, which doesn't exist on Guix.  Repoint at
                 ;; our glibc's ld-linux; the loader then finds its sibling
                 ;; libc/libdl/librt/libpthread in its own directory via the
                 ;; standard "loader's directory" lookup, so no DT_RUNPATH is
                 ;; needed -- NEEDED is limited to those four.  Verified by
                 ;; running `android info' against a binary patched this way
                 ;; and nothing else.  We deliberately do not `--set-rpath'
                 ;; here: rewriting sections of an 85 MB binary that carries
                 ;; an embedded JRE payload buys nothing and risks the
                 ;; segment-offset corruption documented for claude-code in
                 ;; (johnlepikhin packages ai).
                 (invoke patchelf "--set-interpreter"
                         (string-append glibc-lib "/ld-linux-x86-64.so.2")
                         real)

                 ;; Shell wrapper.
                 ;;
                 ;; LD_LIBRARY_PATH is kept deliberately narrow: it covers
                 ;; exactly the NEEDED entries of the shared objects in the
                 ;; JRE the binary unpacks under $ANDROID_USER_HOME (AWT,
                 ;; fontmanager and sound pull in freetype, alsa, X11 and
                 ;; wayland; they are dlopen'd from a directory we cannot
                 ;; patchelf at build time).  glibc and libstdc++ are
                 ;; *omitted* on purpose -- this variable is inherited by
                 ;; every child process, and `android' spawns ./gradlew, adb
                 ;; and the emulator, whose own vendored libraries would be
                 ;; overridden by an ambient glibc ahead of their DT_RUNPATH.
                 (let ((jre-libs
                        (string-join
                         (list #$(file-append freetype "/lib")
                               #$(file-append fontconfig "/lib")
                               #$(file-append alsa-lib "/lib")
                               #$(file-append libx11 "/lib")
                               #$(file-append libxext "/lib")
                               #$(file-append libxrender "/lib")
                               #$(file-append libxkbcommon "/lib")
                               #$(file-append wayland "/lib"))
                         ":"))
                       (tools
                        (string-join
                         (list #$(file-append git-minimal "/bin")
                               #$(file-append coreutils "/bin"))
                         ":")))
                   (call-with-output-file (string-append bin "/android")
                     (lambda (port)
                       (format port "#!~a~%~
                                    export LD_LIBRARY_PATH=~a~%~
                                    export PATH=\"$PATH:~a\"~%~
                                    exec ~a \"$@\"~%"
                               bash jre-libs tools real))))
                 (chmod (string-append bin "/android") #o755)))))
    (native-inputs
     (list patchelf))
    (inputs
     (list alsa-lib
           bash-minimal
           coreutils
           fontconfig
           freetype
           git-minimal
           glibc
           libx11
           libxext
           libxkbcommon
           libxrender
           wayland))
    (supported-systems '("x86_64-linux"))
    (home-page "https://developer.android.com/tools/agents/android-cli")
    (synopsis "Google command line interface for Android development")
    (description
     "Android CLI is Google's command line entry point for Android
development, designed to be driven by AI coding agents as well as by hand.  It
creates projects from templates, installs and updates SDK packages, manages
virtual devices, builds and deploys applications, dumps the UI layout tree,
captures device screenshots, searches the official documentation and installs
@dfn{Android skills} -- curated, up-to-date instructions that coding agents
read before touching Android code.

The binary embeds its own Java runtime, so no external JDK is needed to run
the tool itself; Gradle builds still require one.  SDK components are
downloaded at run time into @env{ANDROID_HOME} and are not managed by Guix.")
    ;; Governed by the Android Software Development Kit License Agreement.
    (license (nonfree "https://developer.android.com/studio/terms"))))
