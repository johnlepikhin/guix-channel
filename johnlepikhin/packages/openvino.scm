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

(define-module (johnlepikhin packages openvino)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages oneapi)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages xml)
  #:use-module (guix build-system cmake)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix search-paths)
  #:use-module (guix utils))

;; OpenVINO's `src/plugins/intel_npu/cmake/download_compiler_libs.cmake`
;; fetches a prebuilt NPU compiler tarball from openvinotoolkit's
;; storage at build time, then renames `libnpu_driver_compiler.so` to
;; `libopenvino_intel_npu_compiler.so` and installs it next to the
;; intel_npu plugin.  Without that file the NPU plugin's dlopen at
;; `compile_model()` fails with `Cannot load library
;; libopenvino_intel_npu_compiler.so`.
;;
;; In the Guix sandbox there is no network, so the upstream download
;; quietly no-ops (the `lsb_release` probe returns empty and skips
;; the whole Linux branch).  Stage the tarball as a regular `(origin)`
;; and have a post-install phase do the rename + place the file at
;; the expected path under `runtime/lib/intel64/`.
(define %openvino-npu-compiler-source
  (origin
    (method url-fetch)
    (uri (string-append "https://storage.openvinotoolkit.org"
                        "/dependencies/thirdparty/linux"
                        "/npu_compiler_vcl_ubuntu_24_04-7_6_0-da3cc32.tar.gz"))
    (file-name "openvino-npu-compiler-vcl-7.6.0.tar.gz")
    (sha256
     (base32 "1an5fd88059k8ny4y6j4m0sgnx8b2lg5z9z953x5dhngk1bzvdn7"))))

;; Intel OpenVINO Toolkit -- inference runtime for CPU/GPU/NPU.
;;
;; This package builds the CPU plugin only.  GPU (-DENABLE_INTEL_GPU=ON)
;; depends on intel-compute-runtime, NPU (-DENABLE_INTEL_NPU=ON) on
;; intel-npu-driver/firmware -- both will land as separate variants
;; once their prerequisites are packaged.
;;
;; Source-side caveats:
;;  - OpenVINO bundles oneDNN, ComputeLibrary, libxsmm, mlas and a few
;;    other deps as git submodules; we fetch with recursive? #t so
;;    submodule trees are in the source.  There is no
;;    ENABLE_SYSTEM_ONEDNN flag upstream.
;;  - System TBB / pugixml / protobuf / flatbuffers are honoured via
;;    -DENABLE_SYSTEM_*=ON.  Other deps (nlohmann_json, ittapi, xbyak,
;;    cnpy, ...) are picked up via find_package or remain bundled.
;;  - Frontends other than ONNX and IR are disabled: FluxFrame, the
;;    consumer that drives this packaging, only needs ONNX inference.
;;
;; OPENVINO_INSTALL_DIR is exported so the Rust `openvino-finder`
;; crate (and pkg-config) locate the runtime without LD_LIBRARY_PATH
;; gymnastics on the consumer side.
(define-public openvino
  (package
    (name "openvino")
    (version "2026.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/openvinotoolkit/openvino")
             (commit version)
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19ak0dr3jpp9a497scdi9vv39d921lc49bmnhlyr7j2h7ph99kmj"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:configure-flags
      #~(list
         ;; Plugins
         "-DENABLE_INTEL_CPU=ON"
         "-DENABLE_INTEL_GPU=OFF"
         "-DENABLE_INTEL_NPU=OFF"
         "-DENABLE_INTEL_NPU_INTERNAL=OFF"
         ;; Meta-plugins (small, useful, on by default upstream)
         "-DENABLE_AUTO=ON"
         "-DENABLE_HETERO=ON"
         "-DENABLE_AUTO_BATCH=ON"
         "-DENABLE_MULTI=ON"
         ;; Frontends: only ONNX + IR (the consumer uses ONNX models)
         "-DENABLE_OV_ONNX_FRONTEND=ON"
         "-DENABLE_OV_IR_FRONTEND=ON"
         "-DENABLE_OV_PADDLE_FRONTEND=OFF"
         "-DENABLE_OV_PYTORCH_FRONTEND=OFF"
         "-DENABLE_OV_JAX_FRONTEND=OFF"
         "-DENABLE_OV_TF_FRONTEND=OFF"
         "-DENABLE_OV_TF_LITE_FRONTEND=OFF"
         ;; Threading: pin to TBB (default TBB_ADAPTIVE bundles its
         ;; own copy when the runtime check fails on a Guix sandbox).
         "-DTHREADING=TBB"
         "-DENABLE_INTEL_OPENMP=OFF"
         "-DENABLE_TBBBIND_2_5=OFF"
         ;; Use Guix-provided libraries instead of bundled copies.
         "-DENABLE_SYSTEM_TBB=ON"
         "-DENABLE_SYSTEM_PUGIXML=ON"
         "-DENABLE_SYSTEM_PROTOBUF=ON"
         ;; OpenVINO sets `Protobuf_USE_STATIC_LIBS=ON` by default
         ;; when ENABLE_SYSTEM_PROTOBUF is on, because Debian/Ubuntu
         ;; ship libprotobuf.a without -fPIC.  Guix's `protobuf`
         ;; output exposes shared, PIC-built libs only, so the
         ;; static-lookup fails with "missing: Protobuf_LIBRARIES".
         ;; Override.
         "-DProtobuf_USE_STATIC_LIBS=OFF"
         "-DENABLE_SYSTEM_FLATBUFFERS=ON"
         ;; Off-by-default but cheap to set explicitly so an upstream
         ;; default flip doesn't silently turn things on:
         "-DENABLE_PYTHON=OFF"
         "-DENABLE_JS=OFF"
         "-DENABLE_TESTS=OFF"
         "-DENABLE_SAMPLES=OFF"
         "-DENABLE_DOCS=OFF"
         "-DCMAKE_SKIP_INSTALL_RPATH=ON")
      #:build-type "Release"
      #:phases
      #~(modify-phases %standard-phases
          ;; Upstream installs CMake config files under `runtime/cmake/`
          ;; and the pkg-config file under `runtime/lib/intel64/pkgconfig/`,
          ;; neither of which CMake's `find_package` or Guix's
          ;; `pkg-config` search-path looks at.  Add thin shims at the
          ;; standard locations that delegate to the upstream files;
          ;; the shimmed `include()` keeps CMAKE_CURRENT_LIST_DIR set
          ;; to the real file inside the included scope, so the
          ;; relative paths in `OpenVINOConfig.cmake` /
          ;; `OpenVINOTargets.cmake` still resolve correctly.
          (add-after 'install 'add-standard-cmake-and-pkgconfig-shims
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (cmake-shim (string-append out "/lib/cmake/OpenVINO"))
                     (pkg-shim (string-append out "/lib/pkgconfig")))
                (mkdir-p cmake-shim)
                (call-with-output-file
                    (string-append cmake-shim "/OpenVINOConfig.cmake")
                  (lambda (port)
                    (display "\
include(\"${CMAKE_CURRENT_LIST_DIR}/../../../runtime/cmake/OpenVINOConfig.cmake\")
"
                             port)))
                (call-with-output-file
                    (string-append cmake-shim "/OpenVINOConfig-version.cmake")
                  (lambda (port)
                    (display "\
include(\"${CMAKE_CURRENT_LIST_DIR}/../../../runtime/cmake/OpenVINOConfig-version.cmake\")
"
                             port)))
                ;; Copy (not symlink) the .pc file and rewrite the
                ;; ${pcfiledir} relative path: the upstream file lives
                ;; 4 directories deep (runtime/lib/intel64/pkgconfig)
                ;; and uses `${pcfiledir}/../../../../`; a symlink
                ;; would let pkg-config resolve `${pcfiledir}` to the
                ;; symlink's location (lib/pkgconfig, only 2 deep) and
                ;; the four `..` segments would walk past the store
                ;; root.  Copying lets us keep the file form intact
                ;; while fixing the depth.
                (mkdir-p pkg-shim)
                (let ((shim-pc (string-append pkg-shim "/openvino.pc")))
                  (copy-file (string-append out "/runtime/lib/intel64"
                                            "/pkgconfig/openvino.pc")
                             shim-pc)
                  (chmod shim-pc #o644)
                  (substitute* shim-pc
                    ;; Upstream uses `pc_path=${pcfiledir}` and then
                    ;; `prefix=${pc_path}/../../../../` -- four `..`s
                    ;; for the original 4-deep location.  Our shim
                    ;; lives at `lib/pkgconfig`, two deep, so two
                    ;; `..`s reach the store prefix.
                    (("prefix=\\$\\{pc_path\\}/\\.\\./\\.\\./\\.\\./\\.\\./")
                     "prefix=${pc_path}/../../"))))))
          ;; Some plugins (e.g. ONNX frontend) ship only the SONAME and
          ;; the real file, but no unversioned `.so` symlink — which
          ;; means `pkg-config --libs openvino` returns `-lopenvino`
          ;; that the linker resolves correctly, yet `dlopen
          ;; ("libopenvino_ir_frontend.so", ...)` from third-party
          ;; loaders (Rust `openvino` crate) fails.  Make every
          ;; versioned plugin have an unversioned symlink.
          (add-after 'add-standard-cmake-and-pkgconfig-shims
                     'ensure-unversioned-symlinks
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (libdir (string-append out "/runtime/lib/intel64")))
                (for-each
                 (lambda (so)
                   (let* ((base (basename so))
                          ;; "libopenvino_ir_frontend.so.2026.1.0"
                          ;; -> "libopenvino_ir_frontend"
                          (stem (substring base 0
                                           (string-contains base ".so."))))
                     (let ((unversioned
                            (string-append libdir "/" stem ".so")))
                       (unless (file-exists? unversioned)
                         (symlink base unversioned)))))
                 (find-files libdir "\\.so\\.[0-9].*$")))))
          ;; OpenVINO installs every plugin and frontend lib into
          ;; `runtime/lib/intel64/` and links them with NEEDED entries
          ;; like `libopenvino.so.2610` -- expecting the sibling
          ;; libraries in the same directory to resolve at dlopen
          ;; time.  Guix's `cmake-build-system` defaults
          ;; `CMAKE_INSTALL_RPATH` to `$out/lib` (so RUNPATH only
          ;; points at the (empty) top-level lib/) and never sees
          ;; `runtime/lib/intel64`, so the libs cannot find each
          ;; other.  Add `$ORIGIN` to every lib's RUNPATH after
          ;; install via patchelf -- robust regardless of how
          ;; upstream and `cmake-build-system` collaborate on RPATH.
          (add-after 'add-standard-cmake-and-pkgconfig-shims
                     'add-origin-rpath
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (libdir (string-append out "/runtime/lib/intel64")))
                (for-each
                 (lambda (so)
                   ;; --add-rpath appends so the toolchain-injected
                   ;; entries (glibc, gcc-lib, tbb, pugixml) stay.
                   (invoke "patchelf" "--add-rpath" "$ORIGIN" so))
                 ;; Filter to real files only -- find-files with
                 ;; `#:stat lstat` returns both symlinks and the
                 ;; final target; without the filter patchelf
                 ;; would follow each symlink and re-patch the same
                 ;; underlying ELF, accumulating duplicate $ORIGIN
                 ;; entries.
                 (filter (lambda (f)
                           (eq? 'regular (stat:type (lstat f))))
                         (find-files libdir "\\.so(\\.[0-9].*)?$"
                                     #:stat lstat))))))
          ;; OpenVINO's `download_compiler_libs.cmake` would normally
          ;; fetch the NPU compiler tarball from openvinotoolkit's
          ;; storage at build time and rename it to
          ;; `libopenvino_intel_npu_compiler.so` next to the NPU
          ;; plugin.  No network in the Guix sandbox; do it ourselves
          ;; once the NPU plugin is built.  Only runs for variants
          ;; that actually built the plugin (CPU-only base openvino
          ;; skips this branch, no point dragging a 120 MB blob).
          (add-after 'add-origin-rpath 'install-npu-compiler
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (libdir (string-append out "/runtime/lib/intel64"))
                     (plugin (string-append libdir
                                            "/libopenvino_intel_npu_plugin.so")))
                (when (file-exists? plugin)
                  (let* ((work (string-append (getcwd) "/.npu-vcl-extract"))
                         (compiler-dst
                          (string-append
                           libdir "/libopenvino_intel_npu_compiler.so")))
                    (mkdir-p work)
                    (invoke "tar" "xzf" #+%openvino-npu-compiler-source
                            "-C" work)
                    (copy-file (string-append
                                work "/lib/libnpu_driver_compiler.so")
                               compiler-dst)
                    (chmod compiler-dst #o755)
                    ;; Borrow the plugin's already-patched RUNPATH
                    ;; (glibc, gcc-lib, tbb, pugixml, $ORIGIN) via a
                    ;; shell `$(patchelf --print-rpath ...)` so we get
                    ;; all toolchain paths without parsing them here;
                    ;; prepend zlib + zstd which the prebuilt compiler
                    ;; additionally needs but the plugin doesn't.
                    (let ((cmd (string-append
                                "patchelf --set-rpath \"\\$ORIGIN:"
                                (assoc-ref inputs "zlib") "/lib:"
                                (assoc-ref inputs "zstd") "/lib:"
                                "$(patchelf --print-rpath '"
                                plugin "')\" '"
                                compiler-dst "'")))
                      (unless (zero? (system cmd))
                        (error "patchelf --set-rpath failed for"
                               compiler-dst)))))))) )))
    (native-inputs
     (list cmake-minimal patchelf pkg-config python-wrapper))
    (inputs
     (list flatbuffers
           nlohmann-json
           protobuf
           pugixml
           snappy
           tbb
           zlib
           zstd))
    (native-search-paths
     (list (search-path-specification
            (variable "OPENVINO_INSTALL_DIR")
            (files '(""))
            ;; Single-value env var: openvino-finder, setupvars.sh
            ;; and CMake's find_package(OpenVINO) all treat
            ;; OPENVINO_INSTALL_DIR as a path to ONE install prefix.
            ;; Without `(separator #f)` Guix Home aggregates the
            ;; value across packages in colon-separated form, which
            ;; openvino-finder then passes to PathBuf::from -- it
            ;; gets a literal `:`-containing path that doesn't exist
            ;; and the loader fails to find libopenvino_c.so.
            (separator #f))))
    (home-page "https://github.com/openvinotoolkit/openvino")
    (synopsis "Intel OpenVINO inference runtime (CPU)")
    (description
     "Intel OpenVINO Toolkit provides a runtime for deep-learning
inference on Intel hardware.  This package builds the CPU plugin with
the ONNX and IR (OpenVINO IR) frontends.  The GPU and NPU plugins are
disabled here; separate variants of this package add them as their
respective driver stacks land in the channel.")
    (license license:asl2.0)))

;; OpenVINO with the Intel NPU plugin compiled in -- targets the
;; Neural Processing Unit on Meteor Lake, Lunar Lake and Panther Lake
;; SoCs via the in-tree `intel_vpu` kernel module.
;;
;; The NPU plugin links only against `libze_loader.so` from
;; `level-zero` at build time; at runtime, the loader looks up
;; `libze_intel_npu.so.1` via dlopen.  That means the
;; `intel-npu-driver` package must be co-installed in the consuming
;; profile (manifest, system, or `guix shell`) for the NPU device to
;; be reachable -- declaring it as a propagated-input here would
;; force the dependency on every consumer regardless of whether they
;; have an NPU, which is intentionally avoided.  The
;; `intel-npu-firmware` package must in turn be in
;; `(operating-system (firmware ...))` so the kernel module loads
;; the device.
(define-public openvino-with-npu
  (package
    (inherit openvino)
    (name "openvino-with-npu")
    (arguments
     (substitute-keyword-arguments (package-arguments openvino)
       ((#:configure-flags flags)
        #~(append (list "-DENABLE_INTEL_NPU=ON"
                        "-DENABLE_INTEL_NPU_INTERNAL=ON")
                  (filter (lambda (f)
                            (not (or (string-prefix? "-DENABLE_INTEL_NPU=" f)
                                     (string-prefix?
                                      "-DENABLE_INTEL_NPU_INTERNAL=" f))))
                          #$flags)))))
    (inputs
     (modify-inputs (package-inputs openvino)
       (prepend level-zero)))
    (synopsis "Intel OpenVINO inference runtime (CPU + NPU)")
    (description
     "Intel OpenVINO Toolkit with both the CPU plugin and the NPU
plugin built in.  Targets the Intel Neural Processing Unit exposed by
the in-tree @code{intel_vpu} kernel module on Meteor Lake, Lunar Lake
and Panther Lake SoCs.  For the NPU plugin to actually find a device
at runtime, the consuming profile must also contain the
@code{intel-npu-driver} package (so @code{libze_intel_npu.so.1} is on
the loader path) and the system must have @code{intel-npu-firmware} in
its kernel firmware list and grant the user access to
@code{/dev/accel/accel0}.")))

;; OpenVINO with all three plugins (CPU + GPU + NPU) compiled in.
;; The GPU plugin uses Intel's Level Zero GPU UMD (libze_intel_gpu.so
;; from intel-compute-runtime), the NPU plugin uses libze_intel_npu.so
;; from intel-npu-driver.  At build time only the Level Zero loader is
;; needed; the actual UMDs are dlopen'd via libze_loader.so at runtime
;; and so they must be co-installed in the consuming profile.
(define-public openvino-full
  (package
    (inherit openvino-with-npu)
    (name "openvino-full")
    (arguments
     (substitute-keyword-arguments (package-arguments openvino-with-npu)
       ((#:configure-flags flags)
        #~(append (list "-DENABLE_INTEL_GPU=ON")
                  (filter (lambda (f)
                            (not (string-prefix? "-DENABLE_INTEL_GPU=" f)))
                          #$flags)))))
    (synopsis "Intel OpenVINO inference runtime (CPU + GPU + NPU)")
    (description
     "Intel OpenVINO Toolkit with the CPU, GPU and NPU plugins all
compiled in.  Pair with @code{intel-compute-runtime} (for the GPU UMD
libze_intel_gpu.so) and @code{intel-npu-driver}
(libze_intel_npu.so.1) in the consuming profile, plus
@code{intel-npu-firmware} in the system's kernel firmware list.  The
GPU plugin transparently falls back to CPU on hosts that don't have an
Intel iGPU/dGPU; the NPU plugin likewise falls back.")))
