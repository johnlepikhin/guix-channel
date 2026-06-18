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
  #:use-module (johnlepikhin packages intel-compute-runtime) ; OCL ICD (для OpenVINO GPU plugin)
  #:use-module (johnlepikhin packages onnxruntime) ; onnxruntime
  #:use-module (johnlepikhin packages openvino)    ; openvino
  #:use-module (johnlepikhin packages rust-binary) ; rust-binary-1.88
  #:use-module (johnlepikhin packages rust-crates))

(define-public fluxframe
  (package
    (name "fluxframe")
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/johnlepikhin/fluxframe.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1rglp03k7x6gnw1diqaga9v1bj67bnk2mjjihgbys51fidkp727v"))))
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
                     (ov       (assoc-ref inputs "openvino"))
                     (icr      (assoc-ref inputs "intel-compute-runtime"))
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
                     ;; OpenCL ICD for OpenVINO GPU plugin.  Not
                     ;; required by ORT or wgpu (Vulkan), but cheap
                     ;; and keeps GPU paths consistent.
                     `("OCL_ICD_VENDORS" = (,(string-append icr "/etc/OpenCL/vendors")))))
                 '("fluxframe" "fluxframe-gui")))))
          ;; R5: smoke-test catches regressions like "ORT_DYLIB_PATH
          ;; missing from wrapper" or "wrapper script syntactically
          ;; broken".  Two checks: (1) wrapper contains the literal
          ;; "ORT_DYLIB_PATH" string, (2) `fluxframe check' completes
          ;; in <10 s against an empty preset with testsrc/fakesink.
          (add-after 'wrap-binaries 'smoke-test
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out     (assoc-ref outputs "out"))
                     (wrapper (string-append out "/bin/fluxframe")))
                ;; (1) Wrapper must bake ORT_DYLIB_PATH.  Plain
                ;; substring check via grep keeps the dependency
                ;; surface minimal (grep ships in coreutils which the
                ;; cargo-build-system already pulls).
                (invoke "grep" "-q" "ORT_DYLIB_PATH" wrapper)
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
    (native-inputs (list gcc-toolchain clang pkg-config))
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
                   ;; / OCL_ICD_VENDORS.  Kept in `inputs' rather than
                   ;; `propagated-inputs' because they are dlopen-style
                   ;; runtime libraries, not user-facing tools, and
                   ;; native-search-paths from a profile cannot be
                   ;; relied upon for shepherd daemons (the daemon's
                   ;; env is not seeded via /etc/profile).
                   ;;
                   ;; R3 ABI pin: `ort 2.0.0-rc.12' with feature
                   ;; `api-24' requires ONNX Runtime >= 1.18; this
                   ;; channel ships 1.26.  Bumping `onnxruntime' to a
                   ;; release that drops API level 24 will cause
                   ;; `load-dynamic' to panic with a misleading
                   ;; message — re-pin or upgrade `ort' in the
                   ;; fluxframe workspace together with this input.
                   onnxruntime
                   openvino
                   intel-compute-runtime)
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
libadwaita editor over the daemon's UNIX control socket.")
    (license (list license:expat license:asl2.0))))
