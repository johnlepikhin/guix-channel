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
  #:use-module (guix utils)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages python-xyz)
  #:use-module ((gnu packages machine-learning) #:prefix ml:))

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
       (replace "pybind11" pybind11)))))
