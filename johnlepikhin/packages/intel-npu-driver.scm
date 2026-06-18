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

(define-module (johnlepikhin packages intel-npu-driver)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages oneapi)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (nonguix licenses)
  #:export (%npu-driver-compiler-source))

;; Prebuilt NPU graph compiler (VCL) tarball Intel publishes alongside
;; OpenVINO releases.  Contains `libnpu_driver_compiler.so` (the actual
;; graph compiler -- LLVM + MLIR + OV passes statically linked into one
;; ~95 MB blob) plus its NEEDED `libtbb.so.12`/`libtbbmalloc.so.2`.
;;
;; This is loaded at runtime by two distinct call sites:
;;
;;  1. `libze_intel_npu.so` (the NPU UMD's Level Zero plugin) dlopens
;;     the file *by its real name* `libnpu_driver_compiler.so` from
;;     inside `Vcl::sym()` -- this is the path used in
;;     "compiler-in-driver" (CID) mode, which is the default in
;;     OpenVINO 2026+ and the one the NPU plugin calls via `pfnCreate2`.
;;
;;  2. The OpenVINO NPU plugin's `compiler_impl.cpp` dlopens the file
;;     renamed to `libopenvino_intel_npu_compiler.so` -- this is the
;;     legacy "compiler-in-plugin" (CIP) fallback path.
;;
;; Building this compiler from source would pull in a 50 GB LLVM/MLIR
;; tree (Intel's NPU LLVM fork); upstream's `ENABLE_NPU_COMPILER_BUILD`
;; would do exactly that and is intentionally disabled in
;; `intel-npu-driver`.  Instead we install the prebuilt blob in
;; `intel-npu-driver`'s own `lib/` so the UMD's RUNPATH resolves the
;; dlopen there (CID path).  OpenVINO's `install-npu-compiler` phase
;; installs the same file under the CIP name in its plugin dir.
;;
;; The tarball is built by Intel as part of the OpenVINO release
;; pipeline; its license is OpenVINO's Apache 2.0 (the same license
;; OpenVINO itself ships under).  Exported so `(johnlepikhin packages
;; openvino)` can reuse the same `(origin)` and avoid two separate
;; downloads / hash drifts of the same upstream artefact.
(define %npu-driver-compiler-source
  (origin
    (method url-fetch)
    (uri (string-append "https://storage.openvinotoolkit.org"
                        "/dependencies/thirdparty/linux"
                        "/npu_compiler_vcl_ubuntu_24_04-7_6_0-da3cc32.tar.gz"))
    ;; File-name kept as `openvino-npu-compiler-vcl-7.6.0.tar.gz` for
    ;; backward compatibility with the pre-extracted store path already
    ;; baked into existing `openvino-full` and `openvino-with-npu`
    ;; derivations -- renaming the source forces a 1.5h rebuild of
    ;; OpenVINO with no actual content change.
    (file-name "openvino-npu-compiler-vcl-7.6.0.tar.gz")
    (sha256
     (base32 "1an5fd88059k8ny4y6j4m0sgnx8b2lg5z9z953x5dhngk1bzvdn7"))))

;; Intel NPU (Neural Processing Unit) user-mode driver -- the Level Zero
;; backend that lets OpenVINO and other Level-Zero consumers talk to the
;; Intel VPU/NPU device exposed by the in-tree `intel_vpu` kernel
;; driver (Meteor Lake, Lunar Lake, Panther Lake, ...).
;;
;; The upstream repo also ships the firmware blobs (`firmware/bin/*.bin`)
;; needed by the kernel module; those are non-free and split out into
;; the sibling `intel-npu-firmware` package so this package stays
;; under a free license.
;;
;; The driver itself is built with the in-source compiler / Perfetto /
;; OpenVINO-package paths off: only the UMD + Level-Zero plugin are
;; needed for OpenVINO's NPU plugin (which carries its own copy of the
;; NPU compiler via the `npu_compiler_elf` submodule).
(define-public intel-npu-driver
  (package
    (name "intel-npu-driver")
    (version "1.32.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/intel/linux-npu-driver")
             (commit (string-append "v" version))
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0p63dag9r3bsvgmcvxrqx5w3flqlx17sx11b5c9gyyqkmr5i92fc"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:configure-flags
      #~(list
         ;; Keep the build narrow: just the UMD + Level Zero plugin.
         ;; The NPU compiler lives in OpenVINO's NPU plugin and is
         ;; loaded at runtime; building it here would pull in LLVM
         ;; and double the build time.
         "-DENABLE_NPU_COMPILER_BUILD=OFF"
         "-DENABLE_NPU_PERFETTO_BUILD=OFF"
         "-DENABLE_NPU_ELF_BUILD=OFF"
         "-DENABLE_OPENVINO_PACKAGE=OFF"
         "-DENABLE_OFFLINE_COMPILATION_SUPPORT=OFF"
         "-DENABLE_VALIDATION_BUILD=OFF"
         "-DENABLE_TOOLS_BUILD=OFF"
         ;; The umd/{vpu,level_zero}_driver/unit_tests subtrees are
         ;; gated by their own `SKIP_UNIT_TESTS` knob -- not the
         ;; top-level `ENABLE_VALIDATION_BUILD` one -- and link via
         ;; gtest/gmock against transitive symbols that GCC's LTO
         ;; doesn't keep visible across object boundaries.  Skip
         ;; them: we're not running the test suite anyway.
         "-DSKIP_UNIT_TESTS=ON"
         "-DCMAKE_INTERPROCEDURAL_OPTIMIZATION=OFF")
      #:phases
      #~(modify-phases %standard-phases
          ;; Upstream's `target_link_libraries(ze_intel_npu
          ;; level_zero_driver)` lets the linker drop object files
          ;; from `liblevel_zero_driver.a` whose symbols aren't
          ;; *directly* reachable from the .so's exported `ze*` C API
          ;; (via the `umd/level_zero_driver/api/ze.exports` version
          ;; script).  The C ABI wrappers reach the `L0::*` C++
          ;; classes only through dynamic dispatch / virtual calls
          ;; that the linker can't trace through a static archive,
          ;; so it discards them as "unused".  Result: our build
          ;; produces a 650 KB `libze_intel_npu.so` with 185 dangling
          ;; `L0::*` undefined references where Intel's Ubuntu deb
          ;; produces a 1.3 MB binary with the same code linked in
          ;; (just hidden by the version script).  Force the whole
          ;; archive in via `--whole-archive` so every object file
          ;; from the static lib ends up in the .so.
          (add-after 'unpack 'force-whole-archive-level-zero-driver
            (lambda _
              ;; Both `liblevel_zero_driver.a` (the L0::* C++ classes)
              ;; and `libvpu_driver.a` (the VPU::* helpers reached
              ;; from L0::ElfParser etc.) need the whole-archive
              ;; treatment.  The `--whole-archive` on level_zero_driver
              ;; pulls in everything it directly defines, but
              ;; vpu_driver is linked one level deeper (as
              ;; `target_link_libraries(level_zero_driver
              ;; vpu_driver ...)`) and the transitive dependency goes
              ;; through static-archive resolution again, so its own
              ;; .o files still get dropped unless we wrap it too.
              (substitute*
                  "umd/level_zero_driver/api/CMakeLists.txt"
                (("target_link_libraries\\(\\$\\{TARGET_NAME_L0\\} level_zero_driver\\)")
                 (string-append
                  "target_link_libraries(${TARGET_NAME_L0} "
                  "-Wl,--whole-archive level_zero_driver vpu_driver "
                  "-Wl,--no-whole-archive)")))))
          ;; Upstream's CMake installs `libze_intel_npu.so.1.32.1`
          ;; with SONAME `libze_intel_npu.so.1` but never creates
          ;; the SONAME symlink -- so a Level Zero loader probing
          ;; for `libze_intel_npu.so.1` via dlopen fails to find
          ;; the file.  Add the canonical SONAME and unversioned
          ;; symlinks ourselves.
          (add-after 'install 'add-soname-symlinks
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (libdir (string-append out "/lib")))
                (with-directory-excursion libdir
                  (unless (file-exists? "libze_intel_npu.so.1")
                    (symlink (string-append "libze_intel_npu.so." #$version)
                             "libze_intel_npu.so.1"))
                  (unless (file-exists? "libze_intel_npu.so")
                    (symlink "libze_intel_npu.so.1"
                             "libze_intel_npu.so"))))))
          ;; Place the prebuilt VCL graph compiler next to
          ;; `libze_intel_npu.so` so the UMD's `Vcl::sym()` dlopen
          ;; (CID mode -- the default for `pfnCreate2`) resolves
          ;; `libnpu_driver_compiler.so` through the UMD's RUNPATH
          ;; without any LD_LIBRARY_PATH crutch.  Without this the
          ;; UMD silently returns `ZE_RESULT_ERROR_UNSUPPORTED_FEATURE`
          ;; from inside `Compiler::getCompiledBlob` and the NPU plugin
          ;; reports the cryptic top-level error
          ;;     Level0 pfnCreate2 result: ZE_RESULT_ERROR_UNSUPPORTED_FEATURE
          ;; with no log trail explaining the dlopen failure.
          ;;
          ;; The compiler additionally bundles its own `libtbb.so.12`
          ;; and `libtbbmalloc.so.2` -- copy those as well so the .so
          ;; satisfies its `NEEDED libtbb.so.12` from `$ORIGIN` without
          ;; conflicting with a system TBB that might be a different
          ;; ABI revision than the one Intel built against.
          (add-after 'add-soname-symlinks 'install-npu-driver-compiler
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (libdir (string-append out "/lib"))
                     (umd (string-append libdir
                                         "/libze_intel_npu.so." #$version))
                     (work (string-append (getcwd) "/.npu-vcl-extract")))
                (mkdir-p work)
                (invoke "tar" "xzf" #+%npu-driver-compiler-source "-C" work)
                (for-each
                 (lambda (so)
                   (let ((src (string-append work "/lib/" so))
                         (dst (string-append libdir "/" so)))
                     (copy-file src dst)
                     (chmod dst #o755)))
                 '("libnpu_driver_compiler.so"
                   "libtbb.so.12"
                   "libtbbmalloc.so.2"))
                ;; All three bundled .so's land in this package's
                ;; `lib/` with empty RUNPATH (Intel ships them for an
                ;; FHS distro where glibc lives in `/lib`).  Borrow
                ;; the UMD's already-patched RUNPATH (glibc, gcc-lib,
                ;; level-zero, ...) via `$(patchelf --print-rpath ...)`
                ;; so all toolchain paths flow through to each blob.
                ;; libnpu_driver_compiler.so additionally needs
                ;; `$ORIGIN` (to find its sibling libtbb.so.12) plus
                ;; zlib and zstd's `lib` output (where libzstd.so.1
                ;; lives -- zstd's default `out` ships only
                ;; bin/etc/share, see the `(,zstd "lib")` input).
                (let ((patch-cmd
                       (lambda (extra-prefix so)
                         (let ((cmd (string-append
                                     "patchelf --set-rpath \""
                                     extra-prefix
                                     (assoc-ref inputs "zlib") "/lib:"
                                     (assoc-ref inputs "zstd") "/lib:"
                                     "$(patchelf --print-rpath '"
                                     umd "')\" '"
                                     libdir "/" so "'")))
                           (unless (zero? (system cmd))
                             (error "patchelf --set-rpath failed for" so))))))
                  ;; `$ORIGIN` so libnpu_driver_compiler.so finds its
                  ;; sibling libtbb.so.12 in the same dir.  The two
                  ;; TBB blobs only need the toolchain paths
                  ;; (libstdc++, glibc, gcc_s).
                  (patch-cmd "\\$ORIGIN:" "libnpu_driver_compiler.so")
                  (patch-cmd "" "libtbb.so.12")
                  (patch-cmd "" "libtbbmalloc.so.2"))))))))
    ;; Upstream's firmware/CMakeLists.txt declares the firmware
    ;; install with `EXCLUDE_FROM_ALL`, so the default `make install`
    ;; in cmake-build-system skips the (non-free, absolute-path) blob
    ;; install rule for us -- no source patch needed.  Build artefacts
    ;; landing in this package's `out` are the UMD + Level Zero
    ;; plugin only; the blobs live in `intel-npu-firmware`.
    (native-inputs
     (list cmake-minimal patchelf pkg-config))
    (inputs
     ;; `zstd "lib"` is needed for libzstd.so.1 (referenced by NEEDED
     ;; in libnpu_driver_compiler.so); zlib is borrowed from existing
     ;; OpenVINO compiler RPATH pattern for safety (defensive against
     ;; any dlopen the compiler may do internally).  zstd's shared
     ;; library lives in the `lib` output -- the default `out` ships
     ;; only bin/etc/share, so without `(,zstd "lib")` we'd reference
     ;; a path with no libzstd.so.1 in it.  The auto-generated label
     ;; becomes "zstd" (with a lint warning) which is what the
     ;; install-npu-driver-compiler phase asks for via
     ;; `(assoc-ref inputs "zstd")`.
     (list level-zero zlib `(,zstd "lib")))
    (home-page "https://github.com/intel/linux-npu-driver")
    (synopsis "Intel NPU user-mode driver (Level Zero backend)")
    (description
     "Intel Linux NPU Driver provides the user-mode driver for Intel
Neural Processing Units (NPU/VPU) exposed by the in-tree
@code{intel_vpu} kernel module.  It implements the Level Zero
backend used by OpenVINO's NPU plugin to dispatch inference workloads
to the NPU on Meteor Lake, Lunar Lake, and Panther Lake SoCs.  The
non-free firmware blobs required by the kernel module live in the
companion @code{intel-npu-firmware} package.")
    (license license:expat)))

;; Non-free firmware blobs (microcode) for the Intel NPU.  Loaded by
;; the `intel_vpu` kernel module on probe; without them the NPU
;; device is unreachable.  Add this package to
;; `(operating-system (firmware ...))` to expose the blobs to the
;; kernel firmware loader.
(define-public intel-npu-firmware
  (package
    (inherit intel-npu-driver)
    (name "intel-npu-firmware")
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("firmware/bin" "lib/firmware/intel/vpu/"
           #:include-regexp ("\\.bin$")))))
    (native-inputs '())
    (inputs '())
    (synopsis "Non-free firmware blobs for the Intel NPU")
    (description
     "Firmware blobs required by the in-tree @code{intel_vpu} kernel
module to bring up the Intel Neural Processing Unit on Meteor Lake,
Lunar Lake, and Panther Lake SoCs.  Pair with the free
@code{intel-npu-driver} user-mode driver.  Intel ships these blobs
under a redistribution-only license; they are non-free.")
    (license (nonfree
              (string-append
               "https://github.com/intel/linux-npu-driver/blob/v"
               (package-version intel-npu-driver)
               "/firmware/bin/COPYRIGHT")))))
