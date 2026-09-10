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

(define-module (johnlepikhin packages fluxframe)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages commencement)         ; gcc-toolchain
  #:use-module (gnu packages elf)                  ; patchelf
  #:use-module (gnu packages gl)                   ; mesa
  #:use-module (gnu packages glib)                 ; glib
  #:use-module (gnu packages gnome)                ; libadwaita
  #:use-module (gnu packages gstreamer)            ; gstreamer, gst-plugins-base/good
  #:use-module (gnu packages gtk)                  ; gtk (=4), graphene, pango, cairo, gdk-pixbuf
  #:use-module (gnu packages linux)                ; pipewire (для pipewiresink)
  #:use-module (gnu packages llvm)                 ; clang (libclang для bindgen в v4l2-sys-mit)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages vulkan)               ; vulkan-loader, vulkan-headers
  #:use-module (guix build-system cargo)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (johnlepikhin build rust)           ; %rust-cc-symlink-phase
  #:use-module (johnlepikhin packages intel-compute-runtime) ; OCL ICD + L0 GPU UMD
  #:use-module (johnlepikhin packages intel-npu-driver) ; L0 NPU UMD
  #:use-module (johnlepikhin packages onnxruntime) ; onnxruntime
  #:use-module (johnlepikhin packages openvino)    ; openvino-full
  #:use-module (johnlepikhin packages rust-binary) ; rust-binary-1.88
  #:use-module (johnlepikhin packages rust-crates))

(define-public fluxframe
  (package
    (name "fluxframe")
    (version "0.7.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/johnlepikhin/fluxframe.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0c89qfxrslnvvm5757bx77sdbb0d2lxvgsx2qmi6q89fyb1w6asi"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:rust rust-binary-1.88
      #:install-source? #f
      ;; Default is #:tests? #t.  Cargo skips #[ignore] tests
      ;; (e2e/v4l2/ml integration suites) automatically; the remaining
      ;; unit tests in config_merge/control/socket/runtime are pure
      ;; logic and safe inside the build sandbox.
      #:phases
      #~(modify-phases %standard-phases
          #$%rust-cc-symlink-phase
          ;; v4l2-sys-mit и openvino-sys через bindgen дергают libclang
          ;; за разрешением C-заголовков; без LIBCLANG_PATH крейтовый
          ;; build-script падает с "Unable to find libclang".
          (add-before 'configure 'set-libclang-path
            (lambda* (#:key inputs #:allow-other-keys)
              (setenv "LIBCLANG_PATH"
                      (string-append (assoc-ref inputs "clang") "/lib"))))
          ;; Явно устанавливаем оба бинарника workspace'а, чтобы не
          ;; зависеть от того, что cargo-build-system по умолчанию
          ;; разложит multi-bin workspace корректно.
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
                (mkdir-p bin)
                (for-each
                 (lambda (name)
                   (install-file (string-append "target/release/" name) bin))
                 '("fluxframe" "fluxframe-gui")))))
          (add-after 'install 'wrap-binaries
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (gst-base (assoc-ref inputs "gst-plugins-base"))
                     (gst-good (assoc-ref inputs "gst-plugins-good"))
                     (pipewire (assoc-ref inputs "pipewire"))
                     (vulkan   (assoc-ref inputs "vulkan-loader"))
                     (mesa     (assoc-ref inputs "mesa"))
                     (ort      (assoc-ref inputs "onnxruntime"))
                     (ov       (assoc-ref inputs "openvino-full"))
                     (icr      (assoc-ref inputs "intel-compute-runtime"))
                     (npu      (assoc-ref inputs "intel-npu-driver"))
                     (gst-path (string-join
                                (list (string-append gst-base "/lib/gstreamer-1.0")
                                      (string-append gst-good "/lib/gstreamer-1.0")
                                      (string-append pipewire "/lib/gstreamer-1.0"))
                                ":"))
                     (ld-path  (string-join
                                (list (string-append vulkan "/lib")
                                      (string-append mesa   "/lib"))
                                ":")))
                (for-each
                 (lambda (bin)
                   (wrap-program (string-append out "/bin/" bin)
                     `("GST_PLUGIN_SYSTEM_PATH" prefix (,gst-path))
                     `("LD_LIBRARY_PATH" prefix (,ld-path))
                     ;; R1: fluxframe-effects::ml::OnnxEngine uses ort
                     ;; with `load-dynamic'; without ORT_DYLIB_PATH the
                     ;; dlopen probe hangs deep inside ORT's
                     ;; initialisation (no error returned).  Bake the
                     ;; absolute store path here so it works in every
                     ;; launch context (shepherd, CLI, tests).
                     `("ORT_DYLIB_PATH" = (,(string-append ort "/lib/libonnxruntime.so")))
                     ;; R2: OpenVINO is loaded via openvino-rs
                     ;; runtime-linking; the finder honours
                     ;; OPENVINO_INSTALL_DIR.  Failure mode of an
                     ;; unset variable is a clean fallback to ORT, but
                     ;; we wire it for completeness.
                     `("OPENVINO_INSTALL_DIR" = (,ov))
                     ;; OpenCL ICD for the OpenVINO GPU plugin.  This
                     ;; is the load-bearing variable for the GPU path:
                     ;; `libopenvino_intel_gpu_plugin.so' has NEEDED
                     ;; `libOpenCL.so.1' and does *not* go through
                     ;; Level Zero.
                     `("OCL_ICD_VENDORS" = (,(string-append icr "/etc/OpenCL/vendors")))
                     ;; R6: Level Zero driver lookup, i.e. the NPU
                     ;; path.  The patched `libze_loader.so.1' (see
                     ;; patches/level-zero-ze-driver-path-env.patch)
                     ;; reads ZE_DRIVER_PATH before LD_LIBRARY_PATH,
                     ;; iterates `knownDriverNames' (strict sonames
                     ;; `libze_intel_{gpu,npu}.so.1') and dlopens the
                     ;; absolute path it finds, bypassing DT_RUNPATH.
                     ;; level-zero declares ZE_DRIVER_PATH as a
                     ;; native-search-path, but that only aggregates in
                     ;; a profile — a wrapped binary and the shepherd
                     ;; daemon never see it — so bake the store paths.
                     ;; `prefix' rather than `=': UMDs from other
                     ;; vendors present in the profile stay visible;
                     ;; the loader returns the first match, so these
                     ;; two always win for their own sonames, which is
                     ;; the intended deterministic behaviour.
                     ;;
                     ;; intel-compute-runtime's `lib' is listed because
                     ;; it holds the Level Zero GPU UMD
                     ;; (`libze_intel_gpu.so.1'), which the loader
                     ;; probes alongside the NPU one.
                     ;;
                     ;; Deliberately NOT added to LD_LIBRARY_PATH:
                     ;; intel-npu-driver's `lib' also ships bundled
                     ;; `libtbb.so.12' / `libtbbmalloc.so.2', and there
                     ;; is no reason to put that directory on the
                     ;; dlopen path of the whole process.  Note this is
                     ;; hygiene, not a fix for the TBB clash: ld.so
                     ;; resolves NEEDED by soname against already
                     ;; loaded objects, so the prebuilt NPU compiler
                     ;; ends up running against Guix's oneTBB either
                     ;; way.
                     `("ZE_DRIVER_PATH" prefix
                       (,(string-join (list (string-append icr "/lib")
                                            (string-append npu "/lib"))
                                      ":")))))
                 '("fluxframe" "fluxframe-gui")))))
          ;; R5: smoke-test catches regressions like "ORT_DYLIB_PATH
          ;; missing from wrapper", "wrapper script syntactically
          ;; broken", or -- the reason the static accelerator checks
          ;; below exist -- "OpenVINO silently built without the GPU
          ;; and NPU plugins, AUTO:NPU,GPU,CPU degrades to CPU with no
          ;; error anywhere".  All accelerator checks are static: the
          ;; build sandbox has neither /dev/accel/accel0 nor
          ;; /dev/dri/renderD128, so nothing can be exercised for real.
          (add-after 'wrap-binaries 'smoke-test
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out     (assoc-ref outputs "out"))
                     (wrapper (string-append out "/bin/fluxframe"))
                     (ov      (assoc-ref inputs "openvino-full"))
                     (icr     (assoc-ref inputs "intel-compute-runtime"))
                     (npu     (assoc-ref inputs "intel-npu-driver"))
                     (ov-libs (string-append ov "/runtime/lib/intel64"))
                     (require-file
                      (lambda (path what)
                        (unless (file-exists? path)
                          (error (string-append
                                  "fluxframe smoke-test: missing " path
                                  " -- expected from " what)))))
                     ;; `invoke' cannot capture stdout, and pulling in
                     ;; (ice-9 popen) would mean extending the build
                     ;; script's module list for one grep.  Shelling out
                     ;; keeps the phase self-contained; store paths
                     ;; never contain shell metacharacters.
                     (require-rpath
                      (lambda (plugin)
                        (unless (zero?
                                 (system*
                                  "sh" "-c"
                                  (string-append
                                   "patchelf --print-rpath " ov-libs "/" plugin
                                   " | grep -q level-zero")))
                          (error (string-append
                                  "fluxframe smoke-test: RUNPATH of " plugin
                                  " lacks level-zero -- openvino's"
                                  " `add-level-zero-rpath' phase did not run;"
                                  " the plugin cannot dlopen"
                                  " libze_loader.so.1 at runtime"))))))
                ;; (1) Wrapper must bake ORT_DYLIB_PATH.  Plain
                ;; substring check via grep keeps the dependency
                ;; surface minimal (grep is one of the gnu-build-system
                ;; standard packages, so it costs nothing here).
                (invoke "grep" "-q" "ORT_DYLIB_PATH" wrapper)
                ;; (1b) …and the two accelerator lookup variables.
                ;; ZE_DRIVER_PATH drives the NPU path, OCL_ICD_VENDORS
                ;; the GPU one; losing either is invisible at runtime.
                (invoke "grep" "-q" "ZE_DRIVER_PATH" wrapper)
                (invoke "grep" "-q" "OCL_ICD_VENDORS" wrapper)
                ;; (1c) Both accelerator plugins must actually exist.
                ;; The CPU-only `openvino' variant silently omits them.
                (require-file (string-append ov-libs "/libopenvino_intel_gpu_plugin.so")
                              "openvino-full's -DENABLE_INTEL_GPU=ON")
                (require-file (string-append ov-libs "/libopenvino_intel_npu_plugin.so")
                              "openvino-full's -DENABLE_INTEL_NPU=ON")
                ;; (1d) The user-mode drivers must be present under the
                ;; exact sonames the Level Zero loader probes -- see
                ;; `knownDriverNames' in
                ;; patches/level-zero-ze-driver-path-env.patch.  NEO
                ;; ships no unversioned `libze_intel_gpu.so' at all, so
                ;; checking that name would fail every build while
                ;; testing the wrong thing.
                (require-file (string-append icr "/lib/libze_intel_gpu.so.1")
                              "intel-compute-runtime's install")
                (require-file (string-append npu "/lib/libze_intel_npu.so.1")
                              "intel-npu-driver's `add-soname-symlinks' phase")
                ;; (1e) OpenCL ICD registry for the GPU plugin.
                (require-file (string-append icr "/etc/OpenCL/vendors/intel.icd")
                              "intel-compute-runtime's -DOCL_ICD_VENDORDIR")
                ;; (1f) Both plugins dlopen libze_loader.so.1 lazily, so
                ;; the loader's store path only reaches them through the
                ;; RUNPATH that openvino's `add-level-zero-rpath' phase
                ;; adds.  That phase is guarded by two `when's and fails
                ;; silently -- exactly the class of defect this whole
                ;; check set exists for.
                (require-rpath "libopenvino_intel_npu_plugin.so")
                (require-rpath "libopenvino_intel_gpu_plugin.so")
                ;; (2) End-to-end pipeline construction: testsrc +
                ;; fakesink + empty preset.  No camera, no model, no
                ;; ML inference.  Exercises GStreamer init + the
                ;; wrapper's env exports.  HOME must point somewhere
                ;; writable — sandbox $HOME is typically unset.
                (setenv "HOME" (getcwd))
                (call-with-output-file "smoke-preset.toml"
                  (lambda (port)
                    (display "[presets.default]\n" port)))
                (invoke wrapper
                        "check"
                        "--config" "smoke-preset.toml"
                        "--input" "testsrc"
                        "--output" "fakesink")))))))
    ;; patchelf is used by the smoke-test phase to assert that the
    ;; OpenVINO accelerator plugins can still reach libze_loader.so.1.
    (native-inputs (list gcc-toolchain clang patchelf pkg-config))
    (inputs
     (append (list bash-minimal
                   ;; GStreamer stack: build-time pkg-config + runtime via wrap
                   gstreamer gst-plugins-base gst-plugins-good pipewire
                   ;; GTK4 + libadwaita stack for fluxframe-gui
                   gtk libadwaita graphene pango cairo gdk-pixbuf glib
                   ;; Vulkan loader + mesa ICD for wgpu (GPU blur path)
                   vulkan-loader vulkan-headers mesa
                   ;; dlopen runtime dependencies — baked into the
                   ;; wrapper as ORT_DYLIB_PATH / OPENVINO_INSTALL_DIR
                   ;; / OCL_ICD_VENDORS / ZE_DRIVER_PATH.  Kept in
                   ;; `inputs' rather than `propagated-inputs' because
                   ;; they are dlopen-style runtime libraries, not
                   ;; user-facing tools, and native-search-paths from a
                   ;; profile cannot be relied upon for shepherd
                   ;; daemons (the daemon's env is not seeded via
                   ;; /etc/profile).
                   ;;
                   ;; R3 ABI pin: `ort 2.0.0-rc.12' with feature
                   ;; `api-24' requires ONNX Runtime >= 1.18; this
                   ;; channel ships 1.26.  Bumping `onnxruntime' to a
                   ;; release that drops API level 24 will cause
                   ;; `load-dynamic' to panic with a misleading
                   ;; message — re-pin or upgrade `ort' in the
                   ;; fluxframe workspace together with this input.
                   onnxruntime
                   ;; `openvino-full' rather than the CPU-only
                   ;; `openvino': fluxframe asks for AUTO:NPU,GPU,CPU,
                   ;; and AUTO degrades to whatever plugins happen to
                   ;; be present without raising anything.  With the
                   ;; CPU-only variant that degradation is total and
                   ;; invisible.
                   ;;
                   ;; Note openvino-full (and intel-npu-driver below)
                   ;; each carry a ~97 MiB copy of Intel's prebuilt VCL
                   ;; graph compiler — a binary shipped without
                   ;; sources.  See the comment above
                   ;; `%npu-driver-compiler-source' in
                   ;; johnlepikhin/packages/intel-npu-driver.scm for the
                   ;; licensing position (Apache 2.0; the non-free
                   ;; firmware lives in `intel-npu-firmware').
                   openvino-full
                   intel-compute-runtime
                   intel-npu-driver)
             (cargo-inputs 'fluxframe
                           #:module '(johnlepikhin packages rust-crates))))
    (home-page "https://github.com/johnlepikhin/fluxframe")
    (synopsis "Realtime video processing layer (background blur, virtual camera)")
    (description
     "FluxFrame reads a video stream from a camera (or a test source), runs it
through a configurable effect chain (ML segmentation, background blur, image
fill, …), and publishes the result as a virtual camera via v4l2loopback so
Zoom, Meet, OBS and browsers can consume it.  Ships two binaries: the
@command{fluxframe} CLI/daemon and @command{fluxframe-gui}, a GTK4 +
libadwaita editor over the daemon's UNIX control socket.

ML segmentation runs through OpenVINO with the device string
@code{AUTO:NPU,GPU,CPU}, so this package is built against
@code{openvino-full} (CPU, GPU and NPU plugins) and wires up both
driver lookup paths.  Using the NPU additionally requires
@code{intel-npu-firmware} in the system's kernel firmware list and
read/write access to @file{/dev/accel/accel0}; the GPU path requires a
working Intel OpenCL ICD.  AUTO moves on to the next device in the list
when one is unavailable — note that behaviour has only been confirmed
for device discovery, not for a device that initialises and then fails
during model compilation.")
    (license (list license:expat license:asl2.0))))
