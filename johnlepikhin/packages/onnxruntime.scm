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

(define-module (johnlepikhin packages onnxruntime)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix search-paths)
  #:use-module (guix utils)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages python-xyz)
  #:use-module ((gnu packages machine-learning) #:prefix ml:)
  #:use-module (johnlepikhin packages openvino))

;; ONNX 1.21.0 as a static C++ library, suitable for linking into
;; onnxruntime 1.25+ (which bumped its onnx dependency from 1.17 to 1.21).
;; Inherits the cmake-build-system arguments from upstream's
;; onnx-for-onnxruntime; only the source (and thus version) changes.
(define-public onnx-1.21-for-onnxruntime
  (package
    (inherit ml:onnx-for-onnxruntime)
    (version "1.21.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/onnx/onnx")
             (commit "v1.21.0")))
       (file-name (git-file-name "onnx-for-onnxruntime" "1.21.0"))
       (sha256
        (base32 "0fvi7vzj4czfrsnzm51v9qnqcggbbpfii2sbf9x7if0k7isq2pkq"))))))

;; onnxruntime 1.26.0.
;;
;; Compared with the 1.22.0 package in upstream Guix:
;;  - bumps onnx to 1.21.0 (required since onnxruntime 1.25);
;;  - bumps pybind11 to 3.0.2 (required since onnxruntime 1.26);
;;  - drops the 1.22-specific SplitToSequence patch (fixed upstream);
;;  - switches abseil-cpp to the default 20250814.1, matching
;;    onnxruntime 1.26's deps.txt (FIND_PACKAGE_ARGS 20250814);
;;  - rewrites the 'relax-dependencies phase: substitutes mp11 so it
;;    uses system Boost (1.26 switched to FetchContent_Populate, which
;;    ignores FIND_PACKAGE_ARGS and otherwise tries to download mp11);
;;  - skips the Python tests (the bundled tests import huggingface
;;    transformers internals that are not packaged in Guix).
(define-public onnxruntime
  (package
    (inherit ml:onnxruntime)
    (name "onnxruntime")
    (version "1.26.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/microsoft/onnxruntime")
             (commit "v1.26.0")))
       (file-name (git-file-name "onnxruntime" "1.26.0"))
       (sha256
        (base32 "0s7ghxbyis0h1795k7hvp6q3lavmdap1plsd7zv7rs7c7nl3mqmk"))))
    (arguments
     (substitute-keyword-arguments (package-arguments ml:onnxruntime)
       ((#:tests? _ #f) #f)
       ((#:phases phases)
        #~(modify-phases #$phases
            (replace 'relax-dependencies
              (lambda _
                ;; Bypass FetchContent for Eigen and use the system copy.
                (with-output-to-file "cmake/external/eigen.cmake"
                  (lambda _
                    (display "find_package(Eigen3 REQUIRED)\n")))
                ;; onnxruntime 1.26 switched mp11 from
                ;; FetchContent_MakeAvailable to FetchContent_Populate,
                ;; which ignores FIND_PACKAGE_ARGS and unconditionally
                ;; downloads boostorg/mp11 from GitHub.  Force Boost
                ;; (which provides mp11 as a header) to be located via
                ;; find_package and alias it as Boost::mp11.
                (substitute* "cmake/external/onnxruntime_external_deps.cmake"
                  (("if\\(NOT TARGET Boost::mp11\\)" all)
                   (string-append
                    "find_package(Boost REQUIRED)\n"
                    "if(NOT TARGET Boost::mp11)\n"
                    "  add_library(Boost::mp11 ALIAS Boost::headers)\n"
                    "endif()\n"
                    all)))))
            ;; Tests in onnxruntime/python/test depend on huggingface
            ;; transformers internals (e.g. EncoderDecoderCache) that
            ;; are not packaged in Guix, and a stray local "transformers"
            ;; directory shadows the import.  Skip Python tests entirely.
            (delete 'check)
            (delete 'python-sanity-check)))))
    (inputs
     (modify-inputs (package-inputs ml:onnxruntime)
       (replace "abseil-cpp" abseil-cpp)
       (replace "onnx-for-onnxruntime" onnx-1.21-for-onnxruntime)
       ;; onnxruntime 1.26's pybind11.cmake requires pybind11 >= 3.0;
       ;; the upstream Guix recipe still pinned pybind11-2 (2.13.6).
       (replace "pybind11" pybind11)))
    ;; Export `ORT_DYLIB_PATH` as a single-file search-path pointing
    ;; at `<out>/lib/libonnxruntime.so`.  The Rust `ort 2.x` crate
    ;; with the `load-dynamic` feature reads this variable to locate
    ;; the runtime via dlopen at process start; without it FluxFrame
    ;; (and any other ort load-dynamic consumer) hangs in dlopen
    ;; inside `guix shell` since Guix relies on RUNPATH (not
    ;; LD_LIBRARY_PATH) and the consumer binary doesn't directly
    ;; link libonnxruntime.so.
    ;;
    ;; The path resolves to the unversioned `libonnxruntime.so`
    ;; symlink rather than the SONAME-versioned file so it survives
    ;; minor version bumps.  `separator #f` keeps the value
    ;; single-valued; `file-type 'regular` makes Guix point at the
    ;; file directly instead of synthesising the parent directory.
    (native-search-paths
     (list (search-path-specification
            (variable "ORT_DYLIB_PATH")
            (file-type 'regular)
            (separator #f)
            (files (list "lib/libonnxruntime.so")))))))

;; onnxruntime with the OpenVINO Execution Provider compiled in.
;;
;; Kept as a separate variant rather than turning the EP on in the
;; default `onnxruntime` package because:
;;
;;   1. The default `onnxruntime` is a heavy build already (~12 min on
;;      8 cores).  Pulling in `openvino` adds 30-60 min of upstream
;;      compile to every user who only wanted CPU EP.
;;   2. Rolling back is cheap: a regression in the OpenVINO EP doesn't
;;      brick CPU-only users.
;;
;; The Rust crate `ort 2.x` with the `load-dynamic` feature picks the
;; OpenVINO EP up at runtime as long as the libonnxruntime.so it loads
;; was built with `--use_openvino`; the OPENVINO_INSTALL_DIR env var
;; published by the `openvino` package's search-path then lets ORT
;; locate the OpenVINO runtime libraries without LD_LIBRARY_PATH
;; gymnastics.
(define-public onnxruntime-openvino
  (package
    (inherit onnxruntime)
    (name "onnxruntime-openvino")
    (arguments
     (substitute-keyword-arguments (package-arguments onnxruntime)
       ((#:configure-flags flags '())
        #~(append (list "-Donnxruntime_USE_OPENVINO=ON"
                        "-Donnxruntime_USE_OPENVINO_AUTO=ON")
                  #$flags))
       ((#:phases phases)
        #~(modify-phases #$phases
            ;; onnxruntime 1.26's OpenVINO EP wrappers compile with
            ;; -Werror=unused-function / -Werror=unused-but-set-variable
            ;; turned on, but the EP source itself trips both: the
            ;; HETERO/MULTI/AUTO-only `parseDevices` helper is unused
            ;; when only AUTO is requested, and inside it a dead
            ;; `dev_options_update` captures the return value of an
            ;; `std::set::emplace` that's only called for its side
            ;; effect.  Patch both so the EP compiles without lowering
            ;; the global warning gate.
            (add-after 'unpack 'patch-openvino-ep-warnings
              (lambda _
                (substitute*
                    "onnxruntime/core/providers/openvino/openvino_execution_provider.cc"
                  (("auto dev_options_update = dev_options\\.emplace")
                   "dev_options.emplace")
                  (("^static std::vector<std::string> parseDevices")
                   (string-append "[[maybe_unused]] static "
                                  "std::vector<std::string> parseDevices"))))))) ))
    (inputs
     (modify-inputs (package-inputs onnxruntime)
       (prepend openvino)))
    (synopsis
     "ONNX Runtime with the OpenVINO execution provider")))
