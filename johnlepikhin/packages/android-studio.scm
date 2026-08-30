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

(define-module (johnlepikhin packages android-studio)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module ((gnu packages base) #:select (coreutils glibc))
  #:use-module ((nonguix licenses) #:prefix license:))

;; Android Studio, Google's IntelliJ-based IDE for Android.
;;
;; Shipped as a ~1.5 GB tarball of prebuilt binaries: the IDE itself, a
;; bundled JetBrains Runtime (JBR 21 for the Quail line), native helpers
;; (fsnotifier, restarter, lldb) and a pile of per-plugin shared objects.
;; Everything carries `/lib64/ld-linux-x86-64.so.2' as its PT_INTERP, so the
;; install is followed by a blanket patchelf pass.
;;
;; Two build-system knobs are mandatory here, for the same reason they are in
;; (johnlepikhin packages lightburn): this is a self-contained vendor bundle,
;; so `strip' risks corrupting JBR's binaries and `validate-runpath' would
;; fail the build on shared objects that resolve their siblings via $ORIGIN.
;;
;; Known gap: the Python bundled with the NDK plugin's lldb ships _curses and
;; _curses_panel extensions built against the ncurses 5 ABI (libncursesw.so.5,
;; libpanelw.so.5).  Guix only has ncurses 6, so those two modules stay
;; unresolved; they are only reached by lldb's text UI, which nothing in the
;; agent workflow uses.  Every other host binary in the bundle resolves.
;;
;; Note on the SDK: Studio downloads and manages its own SDK under
;; $ANDROID_HOME at run time.  Those binaries are *not* patched by this
;; package -- (johnlepikhin devel android) ships `android-sdk-patch' for
;; that.  Studio also runs its own adb; do not put a second adb on PATH, the
;; two fight over port 5037.
(define-public android-studio
  (package
    (name "android-studio")
    (version "2026.1.3.8")                ;Quail 3 Patch 1
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://edgedl.me.gvt1.com/android/studio/ide-zips/"
                           version "/android-studio-quail3-patch1-linux.tar.gz"))
       (file-name (string-append "android-studio-" version ".tar.gz"))
       ;; sha256 published alongside the download on developer.android.com,
       ;; i.e. from a channel independent of the TLS session that fetches the
       ;; tarball itself.
       (sha256
        (base32 "0py37pqpsxb0x341ay4498pwqn6k1cw42cms5zw16yvldrfyxmav"))))
    (build-system copy-build-system)
    (arguments
     (list
      ;; Prebuilt, self-contained JetBrains bundle: stripping and RUNPATH
      ;; validation make no sense here and only break the vendored runtime.
      #:strip-binaries? #f
      #:validate-runpath? #f
      #:modules '((guix build copy-build-system)
                  (guix build utils)
                  (ice-9 binary-ports)
                  (rnrs bytevectors)
                  (srfi srfi-1)
                  (srfi srfi-13))
      #:install-plan
      #~'(("." "lib/android-studio"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'patch-elf
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((app (string-append #$output "/lib/android-studio"))
                     (interpreter (string-append
                                   (assoc-ref inputs "glibc")
                                   #$(glibc-dynamic-linker)))
                     ;; $ORIGIN leads, and it is not decoration: the bundled
                     ;; llama.cpp, lldb and layoutlib resolve siblings out of
                     ;; their own directory (libggml-base.so.0,
                     ;; libpython3.11.so.1.0, ...), and dropping it in favour
                     ;; of a purely absolute RUNPATH breaks every one of
                     ;; them.  jbr/lib/server holds libjvm.so, which the rest
                     ;; of jbr/lib links against.
                     (rpath (string-join
                             (append (list "$ORIGIN"
                                           ;; bin/ next to lib64/ is how the
                                           ;; bundled lldb is laid out:
                                           ;; LLDBFrontend and
                                           ;; llvm-symbolizer load liblldb
                                           ;; and libc++ from a sibling
                                           ;; directory, not their own.
                                           "$ORIGIN/../lib64"
                                           "$ORIGIN/../lib"
                                           (string-append app "/jbr/lib")
                                           (string-append app "/jbr/lib/server")
                                           (string-append app "/lib")
                                           (string-append app "/bin"))
                                     (map (lambda (label)
                                            (string-append
                                             (assoc-ref inputs label) "/lib"))
                                          '("alsa-lib" "at-spi2-core" "bzip2"
                                            "cups" "dbus" "e2fsprogs"
                                            "fontconfig-minimal"
                                            "freetype" "gcc:lib" "glib"
                                            "glibc" "gtk+" "libbsd"
                                            "libdrm" "libice" "libjpeg-turbo"
                                            "libpng" "libsecret" "libsm" "libx11"
                                            "libxcb" "libxcomposite"
                                            "libxcursor" "libxdamage"
                                            "libxext" "libxfixes" "libxi"
                                            "libxkbcommon" "libxkbfile"
                                            "libxcrypt" "libxml2" "libxrandr"
                                            "libxrender" "libxtst" "mesa"
                                            "ncurses" "nspr" "nss" "openssl"
                                            "pulseaudio" "vulkan-loader"
                                            "wayland" "zlib")))
                             ":")))

                ;; Payloads pushed to the phone or emulator rather than run
                ;; here: the screen-sharing agent, perfetto, the profiler's
                ;; JVMTI agent.  Android's linker resolves those against its
                ;; own namespace, so a store RUNPATH is meaningless and
                ;; rewriting them only alters what gets installed on the
                ;; device.  They are identified by sitting in a directory
                ;; named after an Android ABI -- excluding the whole of
                ;; plugins/android/resources/ would be wrong, since JNI
                ;; helpers such as native/libjni_object_tagger.so and
                ;; simpleperf/linux-x86_64/ live there too and do run here.
                (define %android-abis
                  '("arm64-v8a" "armeabi-v7a" "armeabi" "x86" "x86_64"
                    "riscv64" "mips" "mips64"))

                (define (device-payload? file)
                  (any (lambda (abi)
                         (string-contains file (string-append "/" abi "/")))
                       %android-abis))
                (define (host-elf? file)
                  ;; The bundle is mostly jars, XML and PNGs that have no
                  ;; business reaching patchelf, and it also carries ARM
                  ;; builds of the device payloads.  The 20-byte header
                  ;; settles both: bytes 0-3 are the magic, bytes 18-19 are
                  ;; e_machine (0x3e = EM_X86_64, little endian).  Symlinks
                  ;; are skipped -- their target is visited on its own.
                  (and (eq? 'regular (stat:type (lstat file)))
                       (call-with-input-file file
                         (lambda (port)
                           (let ((header (get-bytevector-n port 20)))
                             (and (bytevector? header)
                                  (= 20 (bytevector-length header))
                                  (= #x7f (bytevector-u8-ref header 0))
                                  (= #x45 (bytevector-u8-ref header 1))
                                  (= #x4c (bytevector-u8-ref header 2))
                                  (= #x46 (bytevector-u8-ref header 3))
                                  (= #x3e (bytevector-u8-ref header 18))
                                  (= #x00 (bytevector-u8-ref header 19)))))
                         #:binary #t)))

                (for-each
                 (lambda (file)
                   (if (false-if-exception
                        (invoke/quiet "patchelf" "--print-interpreter" file))
                       (invoke "patchelf" "--set-interpreter" interpreter
                               "--set-rpath" rpath file)
                       ;; No PT_INTERP means one of two things: a shared
                       ;; object, which just needs the RUNPATH, or a static
                       ;; executable such as bin/fsnotifier, which has no
                       ;; .dynamic section for patchelf to rewrite and needs
                       ;; nothing from us anyway.  Only the former can be
                       ;; told apart by trying.
                       (unless (false-if-exception
                                (invoke/quiet "patchelf" "--set-rpath"
                                              rpath file))
                         (format #t "patch-elf: leaving static ~a alone~%"
                                 file))))
                 (filter (lambda (file)
                           (and (not (device-payload? file))
                                (host-elf? file)))
                         (find-files app ".*" #:stat lstat))))))
          (add-after 'patch-elf 'install-launcher
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((app (string-append #$output "/lib/android-studio"))
                     (bin (string-append #$output "/bin"))
                     (apps (string-append #$output "/share/applications"))
                     (icons (string-append #$output
                                           "/share/icons/hicolor/scalable/apps"))
                     ;; Recent releases ship a native `bin/studio'; older
                     ;; ones only the `bin/studio.sh' shell launcher.
                     (launcher (if (file-exists? (string-append app "/bin/studio"))
                                   (string-append app "/bin/studio")
                                   (string-append app "/bin/studio.sh"))))
                (mkdir-p bin)
                (call-with-output-file (string-append bin "/android-studio")
                  (lambda (port)
                    ;; STUDIO_JDK keeps the IDE on its bundled JetBrains
                    ;; Runtime rather than whatever JDK the profile exports
                    ;; through JAVA_HOME for Gradle.
                    ;;
                    ;; gsettings is not optional: IntelliJ shells out to it
                    ;; while working out whether it can disable the input
                    ;; method, and a missing binary there aborts startup with
                    ;; "Start Failed / Cannot run program gsettings".
                    ;;
                    ;; libsecret (credential store) and libe2p from
                    ;; e2fsprogs (FileSystemUtil, for the ext4 case-
                    ;; sensitivity flag) are both loaded by JNA under bare
                    ;; names, so they are found through the loader's search
                    ;; path rather than any RUNPATH we could patch in.  Only
                    ;; those two directories go into LD_LIBRARY_PATH -- the
                    ;; variable is inherited by Gradle, adb and the emulator,
                    ;; and a broader value would start overriding the
                    ;; libraries they resolve through their own RUNPATH.
                    (format port "#!/bin/sh~@
                                  export STUDIO_JDK=\"~a/jbr\"~@
                                  export PATH=\"~a:~a:$PATH\"~@
                                  export LD_LIBRARY_PATH=\"~a:~a${LD_LIBRARY_PATH:+:\
$LD_LIBRARY_PATH}\"~@
                                  exec \"~a\" \"$@\"~%"
                            app
                            (string-append (assoc-ref inputs "glib:bin") "/bin")
                            (string-append (assoc-ref inputs "coreutils") "/bin")
                            (string-append (assoc-ref inputs "libsecret") "/lib")
                            (string-append (assoc-ref inputs "e2fsprogs") "/lib")
                            launcher)))
                (chmod (string-append bin "/android-studio") #o755)

                (mkdir-p icons)
                (let ((svg (string-append app "/bin/studio.svg"))
                      (png (string-append app "/bin/studio.png")))
                  (when (file-exists? svg)
                    (symlink svg (string-append icons "/android-studio.svg")))
                  (when (file-exists? png)
                    (let ((dir (string-append #$output
                                              "/share/icons/hicolor/256x256/apps")))
                      (mkdir-p dir)
                      (symlink png (string-append dir "/android-studio.png")))))

                (mkdir-p apps)
                (call-with-output-file
                    (string-append apps "/android-studio.desktop")
                  (lambda (port)
                    (format port "[Desktop Entry]~@
                                  Type=Application~@
                                  Name=Android Studio~@
                                  Comment=Integrated development environment for Android~@
                                  Exec=~a/bin/android-studio %f~@
                                  Icon=android-studio~@
                                  Terminal=false~@
                                  Categories=Development;IDE;~@
                                  StartupWMClass=jetbrains-studio~@
                                  StartupNotify=true~%"
                            #$output)))))))))
    (native-inputs (list patchelf))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("at-spi2-core" ,at-spi2-core)
       ;; bzip2 and libxcrypt are for the Python that ships with the bundled
       ;; lldb; its _bz2 and _crypt extension modules need them.
       ("bzip2" ,bzip2)
       ("coreutils" ,coreutils)
       ("cups" ,cups)
       ("dbus" ,dbus)
       ;; libe2p: FileSystemUtil reads the ext4 case-sensitivity flag.
       ("e2fsprogs" ,e2fsprogs)
       ("fontconfig-minimal" ,fontconfig)
       ("freetype" ,freetype)
       ("gcc:lib" ,gcc "lib")
       ("glib" ,glib)
       ;; gsettings lives in the "bin" output, not the default one.
       ("glib:bin" ,glib "bin")
       ("glibc" ,glibc)
       ("gtk+" ,gtk+)
       ("libbsd" ,libbsd)
       ("libdrm" ,libdrm)
       ("libice" ,libice)
       ("libjpeg-turbo" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ;; The credential store loads libsecret through JNA.
       ("libsecret" ,libsecret)
       ("libsm" ,libsm)
       ("libx11" ,libx11)
       ("libxcb" ,libxcb)
       ("libxcomposite" ,libxcomposite)
       ("libxcursor" ,libxcursor)
       ("libxdamage" ,libxdamage)
       ("libxext" ,libxext)
       ("libxfixes" ,libxfixes)
       ("libxi" ,libxi)
       ("libxkbcommon" ,libxkbcommon)
       ;; The emulator's bundled qemu reaches for libxkbfile, libbsd, nspr
       ;; and a flattened nss on top of the usual X11 set.
       ("libxkbfile" ,libxkbfile)
       ("libxcrypt" ,libxcrypt)
       ("libxml2" ,libxml2)
       ("libxrandr" ,libxrandr)
       ("libxrender" ,libxrender)
       ("libxtst" ,libxtst)
       ("mesa" ,mesa)
       ("ncurses" ,ncurses)
       ("nspr" ,nspr)
       ("nss" ,nss)
       ;; The Gemini plugin bundles llama.cpp, whose server links libssl.
       ("openssl" ,openssl)
       ("pulseaudio" ,pulseaudio)
       ("vulkan-loader" ,vulkan-loader)
       ;; JBR's Wayland-native AWT backend and splash screen link against
       ;; libwayland-client directly.
       ("wayland" ,wayland)
       ("zlib" ,zlib)))
    (supported-systems '("x86_64-linux"))
    (home-page "https://developer.android.com/studio")
    (synopsis "Official integrated development environment for Android")
    (description
     "Android Studio is Google's IntelliJ-based IDE for Android development.
It provides the Gradle-backed build system, a layout editor with Compose
preview, profilers, a debugger, the Device Manager for physical and virtual
devices, and Logcat.

This package installs the upstream bundle together with its JetBrains Runtime.
The SDK itself is not included: Studio downloads it at run time into
@env{ANDROID_HOME}, and those binaries need the @command{android-sdk-patch}
helper from @code{(johnlepikhin devel android)} to run on Guix System.")
    ;; Governed by the Android Software Development Kit License Agreement.
    (license (license:nonfree "https://developer.android.com/studio/terms"))))
