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

(define-module (johnlepikhin packages intel-graphics-compiler)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages vulkan)
  #:use-module (guix build-system cmake)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages))

;; Intel Graphics Compiler (IGC) -- the OpenCL / Level Zero / SPIR-V to
;; Gen ISA backend used by the Intel Compute Runtime (NEO).  IGC bundles
;; a patched LLVM 16 toolchain plus the Intel OpenCL Clang frontend and
;; the SPIR-V LLVM Translator; the patches are applied during the IGC
;; build and the resulting compiler is statically linked into NEO's
;; libigdrcl.so (OpenCL ICD) and libze_intel_gpu.so (Level Zero GPU
;; UMD).
;;
;; This recipe follows the Nixpkgs blueprint for IGC -- arrange
;; llvm-project plus three sibling repos into the layout the upstream
;; build expects, point IGC at the system SPIRV-Tools / SPIRV-Headers,
;; and let the in-tree CMake build the bundled LLVM workspace.  Build
;; time is on the order of 2-4h on 8 cores; scratch peak is ~50 GB.
;;
;; The four sibling sources are pinned to the same commits Nixpkgs
;; uses for IGC 2.34.4 (and the same versions the IGC v2.34.4
;; build_ubuntu.md documentation calls out for LLVM 16).
(define %igc-llvm-project-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/llvm/llvm-project")
          (commit "llvmorg-16.0.6")))
    (file-name (git-file-name "llvm-project" "16.0.6"))
    (sha256
     (base32 "0jxmapg7shwkl88m4mqgfjv4ziqdmnppxhjz6vz51ycp2x4nmjky"))))

(define %igc-vc-intrinsics-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/intel/vc-intrinsics")
          (commit "v0.25.0")))
    (file-name (git-file-name "vc-intrinsics" "0.25.0"))
    (sha256
     (base32 "093yn5508xdpym3rrb1i0r27rhr4v7kvjix3qa3saikrfp1kadx3"))))

(define %igc-opencl-clang-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/intel/opencl-clang")
          (commit "v16.0.11")))
    (file-name (git-file-name "opencl-clang" "16.0.11"))
    (sha256
     (base32 "0qqdr16bi8rkni3q4y9wq3lmqn9y9nymavibablws7jc6f6varks"))))

(define %igc-llvm-spirv-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/KhronosGroup/SPIRV-LLVM-Translator")
          (commit "v16.0.24")))
    (file-name (git-file-name "SPIRV-LLVM-Translator" "16.0.24"))
    (sha256
     (base32 "1xdasp1yrj4rh6qkfj56qyvk3fn7zg81vpw8wf0d6nbn1dyk0dv9"))))

(define-public intel-graphics-compiler
  (package
    (name "intel-graphics-compiler")
    (version "2.34.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/intel/intel-graphics-compiler")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02c7ybd3x8ml2x0a8ncwg8nx755qff02sj3hpzpx18zhgnp2fvf3"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:configure-flags
      #~(list "-DIGC_OPTION__SPIRV_TOOLS_MODE=Prebuilds"
              "-DIGC_OPTION__USE_PREINSTALLED_SPIRV_HEADERS=ON"
              (string-append "-DSPIRV-Headers_INCLUDE_DIR="
                             #$(this-package-input "spirv-headers")
                             "/include")
              (string-append "-DLLVM_EXTERNAL_SPIRV_HEADERS_SOURCE_DIR="
                             #$(package-source spirv-headers))
              "-Wno-dev")
      #:phases
      #~(modify-phases %standard-phases
          ;; Multi-source unpack: IGC's CMake searches for the LLVM
          ;; workspace via `${CMAKE_CURRENT_SOURCE_DIR}/../../llvm-project`
          ;; from inside `<igc>/IGC/CMakeLists.txt` -- i.e. a SIBLING of
          ;; the IGC source root in the build directory, not inside it.
          ;; vc-intrinsics uses the same convention.  Drop them as
          ;; peers of `source/` accordingly.
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              (let ((build-root (getcwd)))
                (mkdir "source")
                (chdir "source")
                (copy-recursively source ".")
                (chdir build-root)
                (copy-recursively #$%igc-llvm-project-source "llvm-project")
                (copy-recursively #$%igc-vc-intrinsics-source "vc-intrinsics")
                (copy-recursively #$%igc-opencl-clang-source
                                  "llvm-project/llvm/projects/opencl-clang")
                (copy-recursively #$%igc-llvm-spirv-source
                                  "llvm-project/llvm/projects/llvm-spirv")
                (for-each (lambda (root)
                            (for-each make-file-writable
                                      (find-files root ".*"
                                                  #:directories? #t)))
                          '("source" "llvm-project" "vc-intrinsics"))
                ;; Standard cmake-build-system expects cwd to be the
                ;; source directory when configure starts.
                (chdir "source"))))
          (add-after 'unpack 'patch-source
            (lambda _
              ;; The igc .pc.in uses CMake substitution markers Guix's
              ;; pkg-config sandbox doesn't like; rewrite to literals.
              (substitute*
                  "IGC/AdaptorOCL/igc-opencl.pc.in"
                (("/@CMAKE_INSTALL_INCLUDEDIR@") "/include")
                (("/@CMAKE_INSTALL_LIBDIR@") "/lib"))
              (chmod "IGC/Scripts/igc_create_linker_script.sh" #o755)
              ;; opencl-clang lives outside `source/` (sibling); cmake
              ;; module that drives LLVM patches uses `git am --3way`
              ;; which needs a git history we don't have.  Swap to
              ;; `patch -p1 --ignore-whitespace -i ...`.
              (substitute*
                  (string-append "../llvm-project/llvm/projects/opencl-clang"
                                 "/cmake/modules/CMakeFunctions.cmake")
                (("COMMAND \\$\\{GIT_EXECUTABLE\\} am --3way --keep-non-patch")
                 "COMMAND patch -p1")
                (("--ignore-whitespace -C0 ") "--ignore-whitespace -i "))
              ;; IGC's IRBuilderGenerator drives a `clang -target
              ;; x86_64-pc-windows` BIF cross-compile against IGC's
              ;; own bundled stdint.h (under -nostdinc -isystem).  In
              ;; the LLVM 16 we build with opencl-clang patches
              ;; applied, the `__UINT64_TYPE__` builtin macro resolves
              ;; to `unsigned long` (4 bytes on Windows LLP64) instead
              ;; of `unsigned long long` (8 bytes); struct layouts
              ;; come out 4 bytes short, every `static_assert` on
              ;; `sizeof(...)` or bit-fields wider than 32 bits blows
              ;; up.  Stock clang-16 with the same target shows
              ;; `__UINT64_TYPE__ = long long unsigned int` (8 bytes),
              ;; so the regression is in opencl-clang's patch stack.
              ;;
              ;; Rather than bisect those patches in a 40-min cycle,
              ;; inject extra `-D__INT64_TYPE__=...` flags at the
              ;; cross-compile invocation: clang lets `-D` override
              ;; builtin macros, and the bundled stdint.h then
              ;; typedef's correctly.  Done via the `-Xclang
              ;; -no-opaque-pointers` slot in the script that builds
              ;; IRBuilderGenerator targets.
              ;; Drop a fixup header next to the IGC source root.  It
              ;; defines the 64-bit-wide stdint typedefs ourselves and
              ;; defines `__CLANG_STDINT_H` so the bundled stdint.h
              ;; subsequently included by the BIF .cpp's becomes a
              ;; no-op (its `#ifndef __CLANG_STDINT_H` guard fires).
              (call-with-output-file "igc_bif_stdint_fix.h"
                (lambda (port)
                  (display "\
typedef signed char        int8_t;
typedef unsigned char      uint8_t;
typedef signed short       int16_t;
typedef unsigned short     uint16_t;
typedef signed int         int32_t;
typedef unsigned int       uint32_t;
typedef signed long long   int64_t;
typedef unsigned long long uint64_t;
typedef long long          intmax_t;
typedef unsigned long long uintmax_t;
typedef long long          intptr_t;
typedef unsigned long long uintptr_t;
typedef long long          int_least64_t;
typedef unsigned long long uint_least64_t;
typedef long long          int_fast64_t;
typedef unsigned long long uint_fast64_t;
#define __CLANG_STDINT_H
#define INT64_C(c)  c ## LL
#define UINT64_C(c) c ## ULL
"
                           port)))
              (substitute*
                  "IGC/cmake/IRBuilderGeneratorCodeGen.cmake"
                (("-target x86_64-pc-windows\n")
                 (string-append
                  "-target x86_64-pc-windows-msvc\n"
                  "        -include\n"
                  "        ${CMAKE_SOURCE_DIR}/igc_bif_stdint_fix.h\n"))))))))
    (native-inputs
     (list bison
           cmake-minimal
           flex
           git
           ninja
           python-wrapper
           python-mako
           python-pyyaml))
    (inputs
     (list spirv-headers
           spirv-tools
           zlib
           zstd))
    (home-page "https://github.com/intel/intel-graphics-compiler")
    (synopsis "Intel Graphics Compiler for OpenCL / Level Zero")
    (description
     "The Intel Graphics Compiler (IGC) translates OpenCL, SPIR-V and
Level Zero device programs to Gen ISA for Intel integrated and
discrete GPUs.  It bundles a patched LLVM 16 toolchain together with
the @code{opencl-clang} OpenCL frontend and the SPIR-V LLVM
Translator; consumers link IGC into a runtime such as the Intel
Compute Runtime (NEO), which provides the OpenCL ICD and the Level
Zero GPU UMD.")
    (license license:expat)))
