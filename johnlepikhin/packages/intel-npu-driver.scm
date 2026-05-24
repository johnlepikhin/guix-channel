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
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages oneapi)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (nonguix licenses))

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
         "-DSKIP_UNIT_TESTS=ON")
      #:phases
      #~(modify-phases %standard-phases
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
                             "libze_intel_npu.so")))))))))
    ;; Upstream's firmware/CMakeLists.txt declares the firmware
    ;; install with `EXCLUDE_FROM_ALL`, so the default `make install`
    ;; in cmake-build-system skips the (non-free, absolute-path) blob
    ;; install rule for us -- no source patch needed.  Build artefacts
    ;; landing in this package's `out` are the UMD + Level Zero
    ;; plugin only; the blobs live in `intel-npu-firmware`.
    (native-inputs
     (list cmake-minimal pkg-config))
    (inputs
     (list level-zero))
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
