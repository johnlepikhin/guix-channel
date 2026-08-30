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

(define-module (johnlepikhin devel android)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages image)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (johnlepikhin packages android-cli)
  #:use-module (johnlepikhin packages android-studio)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:export (home-devel-android-configuration
            home-devel-android-configuration?
            home-devel-android-service-type))

;;; Commentary:
;;;
;;; Android development environment for Guix Home.
;;;
;;; The awkward part of Android on Guix System is that everything
;;; `android sdk install' downloads -- platform-tools, build-tools, the
;;; emulator, cmdline-tools -- are prebuilt FHS binaries whose PT_INTERP is
;;; `/lib64/ld-linux-x86-64.so.2', a path that does not exist here.  This
;;; service therefore ships `android-sdk-patch', which repoints those
;;; binaries at the store, and `android-sdk-sync', which installs the
;;; declared component list and patches it in one step.
;;;
;;; What is and isn't reproducible: the `android-cli' and `android-studio'
;;; packages are pinned by hash, but SDK components are not -- Google
;;; publishes no stable per-version URLs with checksums for them.  The
;;; `sdk-components' field records the *intent* (which components, which
;;; versions) so it lives in git alongside home.scm, but the bytes under
;;; $ANDROID_HOME come from the network and `guix home roll-back' does not
;;; revert them.
;;;
;;; Code:

(define-record-type* <home-devel-android-configuration>
  home-devel-android-configuration make-home-devel-android-configuration
  home-devel-android-configuration?
  ;; The Android CLI package providing bin/android.
  (android-cli       home-devel-android-configuration-android-cli
                     (default android-cli))
  ;; The Android Studio package.  Only used when INSTALL-STUDIO? is #t.
  (studio            home-devel-android-configuration-studio
                     (default android-studio))
  (install-studio?   home-devel-android-configuration-install-studio?
                     (default #t))
  ;; JDK for Gradle, as a (PACKAGE OUTPUT) pair.  The default openjdk output
  ;; is a JRE and carries no `javac'; only the "jdk" output does, and AGP
  ;; needs it.  Android CLI itself embeds its own runtime and does not care.
  (jdk               home-devel-android-configuration-jdk
                     (default (list (specification->package "openjdk@17")
                                    "jdk")))
  ;; $ANDROID_HOME.  A leading "~/" is accepted.
  (sdk-root          home-devel-android-configuration-sdk-root
                     (default "~/Android/Sdk"))
  ;; $ANDROID_USER_HOME.
  (android-user-home home-devel-android-configuration-android-user-home
                     (default "~/.android"))
  ;; SDK packages `android-sdk-sync' installs, in `android sdk install'
  ;; syntax (see `android sdk list --all' for the identifiers).
  (sdk-components    home-devel-android-configuration-sdk-components
                     (default '("platform-tools"
                                "build-tools/36.0.0"
                                "platforms/android-36"
                                "emulator")))
  ;; Extra packages, or (PACKAGE OUTPUT) pairs, whose /lib goes into the
  ;; RUNPATH that `android-sdk-patch' writes.  Feed it whatever
  ;; `android-sdk-patch --missing' reports as unresolved.
  (extra-libraries   home-devel-android-configuration-extra-libraries
                     (default '()))
  ;; Install a ~/.local/bin/android wrapper that re-patches the SDK after
  ;; every `android sdk ...' invocation.
  (auto-patch-sdk?   home-devel-android-configuration-auto-patch-sdk?
                     (default #t))
  ;; Opt out of Google's usage-data collection.  Android CLI 1.0 has no
  ;; --no-metrics flag; the only lever is ~/.android/analytics.settings.
  (disable-analytics? home-devel-android-configuration-disable-analytics?
                      (default #t))
  (extra-packages    home-devel-android-configuration-extra-packages
                     (default '())))


;;;
;;; Path helpers.
;;;

(define (relative-directory directory)
  "Normalise DIRECTORY into a path relative to $HOME, dropping a leading
\"~/\" and any leading slash, as required by home-files-service-type."
  (let ((stripped (cond ((string-prefix? "~/" directory) (substring directory 2))
                        ((string-prefix? "/"  directory) (substring directory 1))
                        (else directory))))
    (string-trim-right stripped #\/)))

(define (shell-home-path directory)
  "Return DIRECTORY as a shell expression that expands to an absolute path.
Environment-variable values are emitted double-quoted with `$' left intact
but `~' *not* expanded (see shell-double-quote in (gnu home services)), so a
leading \"~/\" has to become \"$HOME/\" explicitly."
  (cond ((string-prefix? "~/" directory)
         (string-append "$HOME/" (substring directory 2)))
        ((string-prefix? "/" directory) directory)
        (else (string-append "$HOME/" directory))))

(define %sdk-patch-path ".local/bin/android-sdk-patch")
(define %sdk-sync-path  ".local/bin/android-sdk-sync")
(define %android-path   ".local/bin/android")


;;;
;;; Library set used for the RUNPATH that android-sdk-patch writes.
;;;

(define %sdk-libraries
  ;; Shared objects the SDK's prebuilt binaries reach for.  Kept broad on
  ;; purpose: the emulator alone pulls in X11, GL, ALSA and PulseAudio.
  (list zlib ncurses expat libxml2 libbsd
        libx11 libxext libxrender libxi libxtst libxfixes libxcursor
        libxrandr libxcb libxkbcommon
        mesa libdrm fontconfig freetype
        alsa-lib pulseaudio dbus glib
        nspr libpng libjpeg-turbo eudev
        ;; libuuid, wanted by the fontconfig the emulator vendors.
        (list util-linux "lib")))

(define (library-directory item)
  "Return a gexp evaluating to the /lib directory of ITEM, which is either a
package or a (PACKAGE OUTPUT) pair.  Note gexp-input rather than file-append:
the latter only ever names a package's default output, so an item like
\(gcc \"lib\") would silently yield \"…-gcc-14.3.0\" with \"lib\" glued on."
  (let ((package (if (pair? item) (first item) item))
        (output  (if (pair? item) (second item) "out")))
    #~(string-append #$(gexp-input package output) "/lib")))

(define (library-bundle config)
  "Return a single directory holding symlinks to every shared object the SDK
might need.

This exists so that the RUNPATH android-sdk-patch writes is *one* entry
instead of thirty.  patchelf cannot grow a non-PIE ET_EXEC binary: with the
full list spelled out the string ran to nearly two kilobytes and patchelf
either aborted or -- worse -- emitted a segfaulting file.  One store path is
short enough to land in the space upstream already reserved."
  (let ((dirs (append (map library-directory
                           (append (list (list gcc "lib"))
                                   %sdk-libraries
                                   (home-devel-android-configuration-extra-libraries
                                    config)))
                      ;; nss keeps libnss3.so one level down, in lib/nss;
                      ;; the Qt WebEngine the emulator vendors links it.
                      (list (file-append nss "/lib/nss")))))
    (computed-file
     "android-sdk-libraries"
     (with-imported-modules '((guix build utils))
       #~(begin
           (use-modules (guix build utils) (ice-9 ftw))
           (mkdir-p #$output)
           (for-each
            (lambda (dir)
              (when (directory-exists? dir)
                (for-each
                 (lambda (name)
                   (let ((target (string-append #$output "/" name)))
                     ;; First definition wins, so the order of the list above
                     ;; decides who provides a duplicated soname.
                     (unless (file-exists? target)
                       (symlink (string-append dir "/" name) target))))
                 (scandir dir (lambda (name)
                                (and (string-prefix? "lib" name)
                                     (string-contains name ".so")))))))
            (list #$@dirs)))))))

(define (library-path config)
  "Return a gexp evaluating to the RUNPATH suffix android-sdk-patch writes."
  #~(string-append #$(library-bundle config)))


;;;
;;; android-sdk-patch.
;;;

(define (make-sdk-patch-script config)
  (let ((sdk-root (shell-home-path
                   (home-devel-android-configuration-sdk-root config))))
    (computed-file
     "android-sdk-patch"
     #~(begin
         (with-output-to-file #$output
           (lambda _
             (display
              (string-append "\
#!/bin/sh
# Repoint prebuilt Android SDK binaries at the Guix store.
#
# Generated by (johnlepikhin devel android) -- do not edit; change the
# service configuration instead.
set -u

INTERP='" #$(file-append glibc "/lib/ld-linux-x86-64.so.2") "'
PATCHELF='" #$(file-append patchelf "/bin/patchelf") "'
OD='" #$(file-append coreutils "/bin/od") "'
LIBS='" #$(library-path config) "'
SDK=\"${ANDROID_HOME:-" #$sdk-root "}\"

# find/cp/mv/rm/tr/mktemp by store path: the activation hook runs this with
# whatever PATH the home generation happens to have.
PATH='" #$(file-append coreutils "/bin") ":" #$(file-append findutils "/bin") "':$PATH
export PATH

# Only these subtrees hold host binaries.  system-images/, sources/ and
# skins/ are guest images and text: walking them would mean tens of
# thousands of pointless stats, and patching them would be wrong.
SUBDIRS='platform-tools build-tools emulator cmdline-tools ndk'

# ...  but even inside them, anything under a directory named after an
# Android ABI is built for the device, not for this machine: the
# RenderScript runtime under build-tools/*/renderscript/lib/*/x86_64/ is
# copied straight into the APK, and it ships its own libc/libm/liblog.
# Giving those a store RUNPATH points device libraries at host ones.
ABI_DIRS='arm64-v8a armeabi-v7a armeabi x86 x86_64 riscv64 mips mips64'

mode=patch
force=0
verbose=0

usage () {
    cat <<EOF
Usage: android-sdk-patch [OPTION]

  --check      exit non-zero if anything still needs patching
  --dry-run    report what would change, write nothing
  --missing    list unresolved NEEDED entries per binary
  --force      re-patch even files that already look correct
  --verbose    list every file per category
  -h, --help   this message
EOF
}

while [ $# -gt 0 ]; do
    case \"$1\" in
        --check)   mode=check ;;
        --dry-run) mode=dry-run ;;
        --missing) mode=missing ;;
        --force)   force=1 ;;
        --verbose) verbose=1 ;;
        -h|--help) usage; exit 0 ;;
        *) echo \"android-sdk-patch: unknown option: $1\" >&2; usage >&2; exit 2 ;;
    esac
    shift
done

if [ ! -d \"$SDK\" ]; then
    echo \"android-sdk-patch: no SDK at $SDK -- nothing to do.\"
    exit 0
fi

n_patched=0 n_ok=0 n_notelf=0 n_arch=0 n_device=0 n_partial=0 n_failed=0 n_todo=0
failed_list=''

say () { [ \"$verbose\" -eq 1 ] && echo \"  $1 $2\"; return 0; }

# Rewrite RUNPATH wholesale rather than appending: an entry inherited from
# an earlier run may point at a store path that `guix gc' has since
# reclaimed.  \\$ORIGIN entries are kept, and kept first -- the emulator
# ships vendored Qt and libc++ under emulator/lib64.
new_rpath () {
    # $ORIGIN leads whether or not upstream had it, and the ladder covers
    # the emulator's vendored layout, where a library
    # loads siblings from a *neighbouring* directory by plain soname:
    #   lib64/gles_swiftshader/*      -> lib64/libc++.so.1        ($ORIGIN/..)
    #   lib64/qt/plugins/*/*          -> lib64/qt/lib/libQt6*     (../../lib)
    #   lib64/qt/lib/*                -> lib64/libc++.so.1        (../..)
    #   lib64/gles_llvmpipe/*         -> lib64/vulkan/libLLVM.so  (../vulkan)
    #   qemu/linux-x86_64/qemu-*      -> lib64/libtcmalloc*       (../../lib64)
    # The ladder is fixed, and upstream's own $ORIGIN entries are *not*
    # carried over: it is already a superset of what the SDK ships, and
    # appending whatever the file currently holds would re-append the ladder
    # itself on every run, growing the RUNPATH without bound and destroying
    # idempotency.  A constant value is also what lets --check compare.
    _origin='$ORIGIN:$ORIGIN/..:$ORIGIN/../..:$ORIGIN/lib64'
    _origin=$_origin':$ORIGIN/../lib:$ORIGIN/../lib64:$ORIGIN/../vulkan'
    _origin=$_origin':$ORIGIN/../../lib:$ORIGIN/../../lib64:$ORIGIN/../../../lib64'
    echo \"$_origin:$LIBS\"
}

# patchelf 0.18 can abort -- or, worse, silently emit a broken file --
# when growing a non-PIE ET_EXEC binary to fit a longer RUNPATH
# (\"Assertion `splitIndex == -1' failed\" in shiftFile).  emulator/bin64/e2fsck
# is one such file: patchelf exited 0 and produced a segfaulting binary.  So
# its exit status is not evidence of anything; load the result through the
# dynamic loader and only keep it if that works.
verify () {
    \"$INTERP\" --list \"$1\" >/dev/null 2>&1
    _rc=$?
    # An unresolved dependency (ld.so exits 127) is not our problem here:
    # plenty of the emulator's vendored libraries only find their siblings
    # once the emulator itself sets a search path.  What must be caught is
    # patchelf having produced something the loader cannot even parse, and
    # that shows up as death by signal -- 128 + signo, i.e. 139 for SIGSEGV.
    [ \"$_rc\" -lt 128 ] || return 1
    \"$PATCHELF\" --print-rpath \"$1\" >/dev/null 2>&1
}

# Diagnostic for --missing only.  ld.so reports an unresolved dependency in
# two different shapes: the ldd-style \"NAME => not found\" table, and, when it
# gives up on the first hard failure, a single \"error while loading shared
# libraries: NAME: cannot open shared object file\" line.  Only matching the
# former is how a missing libgcc_s.so.1 goes unnoticed.
unresolved () {
    \"$INTERP\" --list \"$1\" 2>&1 | while IFS= read -r _line; do
        case \"$_line\" in
            *'error while loading shared libraries: '*)
                _rest=${_line#*error while loading shared libraries: }
                echo \"${_rest%%:*}\" ;;
            *' => not found'*)
                echo \"${_line%% *}\" ;;
        esac
    done
}

# patchelf writes in place; go through a sibling temp file so an interrupted
# run cannot leave a half-written ELF behind.  rename(2) within the same
# directory is atomic.
patch_file () {
    _f=$1; shift
    _tmp=$_f.android-sdk-patch.$$
    if ! cp -p \"$_f\" \"$_tmp\" 2>/dev/null; then return 1; fi
    if \"$PATCHELF\" \"$@\" \"$_tmp\" 2>/dev/null && verify \"$_tmp\"; then
        if mv -f \"$_tmp\" \"$_f\" 2>/dev/null; then return 0; fi
    fi
    rm -f \"$_tmp\"
    return 1
}

# The candidate list goes through a temp file rather than a pipe on purpose:
# `find ... | while read' would run the loop in a subshell and every counter
# incremented inside it would be discarded at the end, making the summary
# read 0 and --check always succeed.
list=$(mktemp) || exit 1
trap 'rm -f \"$list\"' EXIT INT TERM

for d in $SUBDIRS; do
    [ -d \"$SDK/$d\" ] || continue
    find \"$SDK/$d\" -type f \\( -perm -u+x -o -name '*.so' -o -name '*.so.*' \\) -print
done > \"$list\"

while IFS= read -r f; do
    skip=0
    for abi in $ABI_DIRS; do
        case \"$f\" in
            *\"/$abi/\"*) skip=1; break ;;
        esac
    done
    if [ \"$skip\" -eq 1 ]; then
        n_device=$((n_device+1)); say 'device' \"$f\"; continue
    fi

    # One fork decides both ELF-ness and architecture.  The 20-byte header
    # renders as 40 hex characters: bytes 0-3 are the magic (chars 1-8) and
    # bytes 18-19 are e_machine (chars 37-40), little endian, 3e 00 for
    # EM_X86_64.  Compared by field rather than by one long wildcard pattern
    # -- miscounting the '?' run silently classifies every file as foreign.
    hdr=$(\"$OD\" -An -tx1 -N20 \"$f\" 2>/dev/null | tr -d ' \\n')
    magic=${hdr%${hdr#????????}}
    machine=${hdr#????????????????????????????????????}
    if [ \"$magic\" != '7f454c46' ]; then
        n_notelf=$((n_notelf+1)); continue
    fi
    if [ \"$machine\" != '3e00' ]; then
        n_arch=$((n_arch+1)); say 'arch  ' \"$f\"; continue
    fi

    if [ \"$mode\" = missing ]; then
        miss=$(unresolved \"$f\")
        [ -n \"$miss\" ] && printf '%s: %s\\n' \"$f\" \"$(echo $miss)\"
        continue
    fi

    interp=$(\"$PATCHELF\" --print-interpreter \"$f\" 2>/dev/null || true)

    # Idempotency by exact match, never by a /gnu/store prefix: after a
    # `guix pull' plus `guix gc' the old interpreter is gone, yet a prefix
    # test would still call the file done and never repair it.
    need_interp=0
    if [ -n \"$interp\" ]; then
        if [ \"$force\" -eq 1 ] || [ \"$interp\" != \"$INTERP\" ] || [ ! -e \"$interp\" ]; then
            need_interp=1
        fi
    fi

    # RUNPATH is set unconditionally rather than only where a dependency
    # currently fails to resolve.  Probing first looks tidier but does not
    # work: ld.so aborts on the first missing library instead of listing it,
    # so adb -- which needs libgcc_s.so.1 -- was silently judged fine and
    # then failed to start.  These are ordinary prebuilt ELFs, not the
    # single-file bundles that patchelf mangles, and rewriting the whole
    # RUNPATH is also what makes the SDK survive a glibc change: the
    # expected value moves with the store paths, so a stale file no longer
    # compares equal and gets repaired instead of being skipped.
    want_rpath=$(new_rpath \"$f\")
    have_rpath=$(\"$PATCHELF\" --print-rpath \"$f\" 2>/dev/null || true)
    need_rpath=0
    if [ \"$force\" -eq 1 ] || [ \"$have_rpath\" != \"$want_rpath\" ]; then
        need_rpath=1
    fi

    if [ \"$need_interp\" -eq 0 ] && [ \"$need_rpath\" -eq 0 ]; then
        n_ok=$((n_ok+1)); say 'ok    ' \"$f\"; continue
    fi

    # In the reporting modes, a file only counts as outstanding when it is
    # actually unusable: either its interpreter is wrong, or it no longer
    # loads.  A RUNPATH that merely differs from the ideal is not worth
    # flagging, because for a handful of non-PIE executables it can never be
    # written at all (see patch_file) and --check would then never succeed.
    if [ \"$mode\" != patch ]; then
        if [ \"$need_interp\" -eq 1 ] || ! verify \"$f\"; then
            n_todo=$((n_todo+1)); say 'todo  ' \"$f\"
        else
            n_ok=$((n_ok+1))
        fi
        continue
    fi
    n_todo=$((n_todo+1))

    set --
    [ \"$need_interp\" -eq 1 ] && set -- \"$@\" --set-interpreter \"$INTERP\"
    [ \"$need_rpath\" -eq 1 ] && set -- \"$@\" --set-rpath \"$want_rpath\"

    if patch_file \"$f\" \"$@\"; then
        n_patched=$((n_patched+1)); say 'patch ' \"$f\"
    elif [ \"$need_interp\" -eq 1 ] && patch_file \"$f\" --set-interpreter \"$INTERP\"; then
        # The RUNPATH was what did not fit.  An interpreter-only patch is
        # enough whenever NEEDED is limited to libraries the loader finds
        # next to itself -- emulator/bin64/e2fsck, which needs only libc and
        # libpthread, is the case that forced this branch to exist.
        n_partial=$((n_partial+1)); say 'interp' \"$f\"
    elif [ \"$need_interp\" -eq 0 ] && verify \"$f\"; then
        # Interpreter already right, RUNPATH would not fit, and the file
        # loads as it stands: nothing is broken, so this is not a failure.
        n_partial=$((n_partial+1)); say 'interp' \"$f\"
    else
        n_failed=$((n_failed+1)); failed_list=\"$failed_list$f\\n\"
    fi
done < \"$list\"

case \"$mode\" in
    missing) exit 0 ;;
    check)
        if [ \"$n_todo\" -gt 0 ]; then
            echo \"android-sdk-patch: $n_todo file(s) need patching under $SDK\"
            exit 1
        fi
        echo \"android-sdk-patch: $SDK is up to date.\"
        exit 0 ;;
    dry-run)
        printf 'android-sdk-patch: would patch %s, leave %s, skip %s non-ELF, %s non-x86-64, %s device.\\n' \\
               \"$n_todo\" \"$n_ok\" \"$n_notelf\" \"$n_arch\" \"$n_device\"
        exit 0 ;;
esac

printf 'android-sdk-patch: patched %s (%s interpreter-only), already ok %s, skipped %s non-ELF, %s non-x86-64, %s device, failed %s.\\n' \\
       \"$n_patched\" \"$n_partial\" \"$n_ok\" \"$n_notelf\" \"$n_arch\" \"$n_device\" \"$n_failed\"
if [ \"$n_failed\" -gt 0 ]; then
    printf 'failed:\\n'
    printf \"$failed_list\"
    exit 1
fi
exit 0
"))))
         (chmod #$output #o755)))))


;;;
;;; android-sdk-sync.
;;;

(define (make-sdk-sync-script config)
  (let ((android (file-append
                  (home-devel-android-configuration-android-cli config)
                  "/bin/android"))
        (components (home-devel-android-configuration-sdk-components config)))
    (computed-file
     "android-sdk-sync"
     #~(begin
         (with-output-to-file #$output
           (lambda _
             (display
              (string-append "\
#!/bin/sh
# Bring $ANDROID_HOME in line with the service's sdk-components list, then
# make the result runnable on Guix System.
#
# Generated by (johnlepikhin devel android) -- do not edit.
set -eu

ANDROID='" #$android "'
PATCH=\"$HOME/" #$%sdk-patch-path "\"

\"$ANDROID\" sdk install " #$(string-join components " ") "
exec \"$PATCH\" \"$@\"
"))))
         (chmod #$output #o755)))))


;;;
;;; ~/.local/bin/android wrapper.
;;;

(define (make-android-wrapper config)
  (let ((android (file-append
                  (home-devel-android-configuration-android-cli config)
                  "/bin/android")))
    (computed-file
     "android-wrapper"
     #~(begin
         (with-output-to-file #$output
           (lambda _
             (display
              (string-append "\
#!/bin/sh
# Re-patch the SDK after it changes.
#
# The real binary is invoked by absolute store path, never by name: this
# wrapper lives in ~/.local/bin, which bash.scm puts ahead of the profile in
# PATH, so resolving `android' through PATH would call this script again and
# recurse forever.
#
# `sdk' cannot use exec -- there would be no process left to run the patcher
# afterwards.  Every other subcommand does, and android's exit status is
# passed through untouched either way.
set -u

ANDROID='" #$android "'
PATCH=\"$HOME/" #$%sdk-patch-path "\"

case \"${1:-}\" in
    sdk)
        \"$ANDROID\" \"$@\"
        rc=$?
        if [ \"$rc\" -eq 0 ] && [ -x \"$PATCH\" ]; then
            \"$PATCH\" || true
        fi
        exit \"$rc\"
        ;;
    *)
        exec \"$ANDROID\" \"$@\"
        ;;
esac
"))))
         (chmod #$output #o755)))))


;;;
;;; Service extensions.
;;;

(define (add-packages config)
  (let ((jdk (home-devel-android-configuration-jdk config)))
    (append
     (list (home-devel-android-configuration-android-cli config))
     ;; A (PACKAGE OUTPUT) pair; home-profile-service-type understands both.
     (list jdk)
     (if (home-devel-android-configuration-install-studio? config)
         (list (home-devel-android-configuration-studio config))
         '())
     (home-devel-android-configuration-extra-packages config))))

(define (add-environment-variables config)
  (let* ((sdk  (shell-home-path
                (home-devel-android-configuration-sdk-root config)))
         (user (shell-home-path
                (home-devel-android-configuration-android-user-home config)))
         (jdk  (home-devel-android-configuration-jdk config)))
    ;; Order matters: these are emitted as shell `export' statements in
    ;; sequence, so ANDROID_HOME has to be defined before anything expands
    ;; it.  Values are double-quoted with `$' preserved but `~' left alone,
    ;; which is why shell-home-path spells out $HOME.
    `(("ANDROID_HOME"      . ,sdk)
      ("ANDROID_SDK_ROOT"  . "$ANDROID_HOME")
      ("ANDROID_USER_HOME" . ,user)
      ("ANDROID_AVD_HOME"  . "$ANDROID_USER_HOME/avd")
      ;; gexp-input, not file-append: the latter always resolves to the
      ;; package's default output, which for openjdk is a JRE without javac.
      ("JAVA_HOME"         . ,(gexp-input (first jdk) (second jdk)))
      ;; Appended, never prepended.  platform-tools and cmdline-tools ship
      ;; generically named programs (sqlite3, mke2fs, lint, screenshot2)
      ;; that would otherwise shadow the profile's in every shell.  adb
      ;; still wins: the profile deliberately carries no adb of its own.
      ("PATH" . ,(string-append
                  "$PATH:$ANDROID_HOME/platform-tools"
                  ":$ANDROID_HOME/emulator"
                  ":$ANDROID_HOME/cmdline-tools/latest/bin")))))

(define (add-files config)
  (append
   (list (list %sdk-patch-path (make-sdk-patch-script config))
         (list %sdk-sync-path  (make-sdk-sync-script config)))
   (if (home-devel-android-configuration-auto-patch-sdk? config)
       (list (list %android-path (make-android-wrapper config)))
       '())))

(define (add-activation config)
  (let ((sdk-root   (home-devel-android-configuration-sdk-root config))
        (user-home  (home-devel-android-configuration-android-user-home config))
        (analytics? (home-devel-android-configuration-disable-analytics? config)))
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))
          (let* ((home (getenv "HOME"))
                 (resolve (lambda (path)
                            (if (string-prefix? "~/" path)
                                (string-append home "/" (substring path 2))
                                path)))
                 (sdk  (resolve #$sdk-root))
                 (user (resolve #$user-home)))
            (for-each mkdir-p (list sdk user (string-append user "/avd")))

            ;; Android CLI rewrites analytics.settings itself, so it cannot
            ;; be a home-files symlink into the read-only store -- it has to
            ;; be a real file, and we only seed it when absent so a later
            ;; opt-in by hand is not clobbered on every reconfigure.
            ;;
            ;; The key really is "hasOptedIn"; the CLI writes the file as
            ;; {"userId":...,"hasOptedIn":false,"debugDisablePublishing":
            ;; false,"saltValue":0,"saltSkew":-1} and fills in the fields we
            ;; leave out.  Inventing a userId here would be worse than
            ;; omitting it.
            #$(if analytics?
                  #~(let ((settings (string-append user "/analytics.settings")))
                      (unless (file-exists? settings)
                        (call-with-output-file settings
                          (lambda (port)
                            (display "{\"hasOptedIn\":false}\n" port)))))
                  #~#t)

            ;; A reconfigure that moves glibc leaves every previously
            ;; patched SDK binary pointing at a path `guix gc' may reclaim.
            ;; Re-running the patcher here closes that window; it is
            ;; idempotent and exits 0 on an empty SDK.
            (let ((patch (string-append home "/" #$%sdk-patch-path)))
              (when (and (file-exists? patch)
                         (directory-exists? sdk))
                (system* patch))))))))

(define home-devel-android-service-type
  (service-type
   (name 'home-devel-android)
   (extensions
    (list
     (service-extension home-profile-service-type
                        add-packages)
     (service-extension home-environment-variables-service-type
                        add-environment-variables)
     (service-extension home-files-service-type
                        add-files)
     (service-extension home-activation-service-type
                        add-activation)))
   (default-value (home-devel-android-configuration))
   (description "Install and configure an Android development environment:
Android CLI, Android Studio and a JDK for Gradle, plus the environment
variables Android tooling expects.  Because the Android SDK ships prebuilt
FHS binaries that cannot run on Guix System unpatched, the service also
installs @command{android-sdk-patch}, which repoints them at the store, and
@command{android-sdk-sync}, which installs the declared SDK components and
patches them in one go.")))
