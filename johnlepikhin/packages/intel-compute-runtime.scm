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

(define-module (johnlepikhin packages intel-compute-runtime)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages elf)
  #:use-module ((gnu packages oneapi) #:prefix gnu:)
  #:use-module (gnu packages opencl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages video)
  #:use-module (guix build-system cmake)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix search-paths)
  #:use-module (johnlepikhin packages intel-graphics-compiler)
  #:use-module ((nongnu packages video) #:prefix nongnu:))

;; Guix ships level-zero 1.27.0; intel-compute-runtime 26.18 uses
;; `ZE_COMMAND_LIST_FLAG_COPY_OFFLOAD_HINT` (added in 1.28).  Bump.
;;
;; Additionally, patch the Linux driver-discovery loop so it honours
;; a Guix-friendly `ZE_DRIVER_PATH` env var alongside the existing
;; `LD_LIBRARY_PATH` lookup.  Upstream's hard-coded FHS list (`/lib`,
;; `/usr/lib/x86_64-linux-gnu`, ...) is empty on Guix System, so the
;; loader never finds `libze_intel_*.so.1` even when the matching
;; UMD packages are in the profile.  `ZE_DRIVER_PATH` is then
;; declared as a `native-search-paths` for `files=lib`, so every
;; package in the profile contributes its store-path's `lib/` to the
;; UMD search list without touching `LD_LIBRARY_PATH` (which would
;; pollute every other dlopen on the system).
(define-public level-zero
  (package/inherit gnu:level-zero
    (version "1.28.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/oneapi-src/level-zero")
             (commit "v1.28.6")))
       (file-name (git-file-name "level-zero" "1.28.6"))
       (sha256
        (base32 "0k0iliwadklj4ljbjbdqq3ccj6h213917ljfpsrspkjyzqxkhs24"))
       (patches
        (list (local-file
               "patches/level-zero-ze-driver-path-env.patch")))))
    (native-search-paths
     (list (search-path-specification
            (variable "ZE_DRIVER_PATH")
            (files (list "lib")))))))

;; nonguix ships gmmlib 22.8.0; the intel-compute-runtime 26.18 source
;; tree references `IGFX_XE3P_CORE`, `IGFX_CRI` and `IGFX_NVL` -- enum
;; entries for Panther Lake and recent Xe3 derivatives that were only
;; added in gmmlib 22.10.0.  Bump our own copy via package/inherit;
;; the upstream package definition (build system, inputs, license) is
;; otherwise reused as-is.
(define-public gmmlib
  (package/inherit nongnu:gmmlib
    (version "22.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/intel/gmmlib")
             (commit (string-append "intel-gmmlib-22.10.0"))))
       (file-name (git-file-name "gmmlib" "22.10.0"))
       (sha256
        (base32 "0bl1yzk159j6803g12qivk8bsh44wqd0ywpmi0jgbajspv6pv4fl"))))))

;; Intel Compute Runtime (NEO) -- OpenCL ICD and Level Zero GPU UMD
;; for Intel integrated and discrete GPUs.  Provides:
;;   * libigdrcl.so -- the OpenCL ICD vendor driver (loaded via
;;     ocl-icd's /etc/OpenCL/vendors/intel.icd)
;;   * libze_intel_gpu.so -- the Level Zero GPU UMD (loaded by
;;     libze_loader.so via dlopen)
;;
;; Build is fast (~30 min) once intel-graphics-compiler is in place.
;; The trickier part is the ICD installation -- NEO writes
;; intel.icd into /etc/OpenCL/vendors with the absolute path to
;; libigdrcl.so, but we want it inside the package output instead so
;; ocl-icd can pick it up via OCL_ICD_VENDORS env var.
(define-public intel-compute-runtime
  (package
    (name "intel-compute-runtime")
    (version "26.18.38308.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/intel/compute-runtime")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19bq4ffkdq8vnbfqh9iw0c1c44hcfh8c2srclx2ck1fg1jmm6zz7"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:configure-flags
      #~(list "-DSKIP_UNIT_TESTS=ON"
              "-DSKIP_ALL_ULT=ON"
              "-DNEO_DISABLE_MITIGATIONS=ON"
              "-DCMAKE_INSTALL_LIBDIR=lib"
              (string-append "-DOCL_ICD_VENDORDIR="
                             #$output "/etc/OpenCL/vendors")
              ;; Pin both this package's own `lib/` (so ocloc finds
              ;; libocloc.so without `$ORIGIN/../lib`, which the shell
              ;; would expand to empty before reaching the linker) and
              ;; IGC's `lib/` (so libigdrcl.so / libze_intel_gpu.so
              ;; can dlopen libigc / libigdfcl at runtime).
              (string-append "-DCMAKE_BUILD_RPATH="
                             #$output "/lib:"
                             #$(this-package-input
                                "intel-graphics-compiler")
                             "/lib")
              (string-append "-DCMAKE_INSTALL_RPATH="
                             #$output "/lib:"
                             #$(this-package-input
                                "intel-graphics-compiler")
                             "/lib")
              ;; NEO's `cmake/ocloc_cmd_prefix.cmake` wraps every ocloc
              ;; invocation in `cmake -E env LD_LIBRARY_PATH=...` and
              ;; appends `${NEO__IGC_LIBRARY_PATH}` to that path when
              ;; defined.  Without it ocloc cannot dlopen
              ;; `libigdfcl.so.2` and built-in kernel compilation
              ;; fails with FCL initialization failure (-6).
              (string-append "-DNEO__IGC_LIBRARY_PATH="
                             #$(this-package-input
                                "intel-graphics-compiler")
                             "/lib"))
      #:phases
      #~(modify-phases %standard-phases
          ;; Despite `CMAKE_BUILD_RPATH` / `CMAKE_INSTALL_RPATH` cmake
          ;; flags, NEO's installed binaries end up with only the
          ;; toolchain-provided RUNPATH (glibc, gcc-lib).  Patch in
          ;; the missing entries directly with patchelf so
          ;; `validate-runpath` is happy and ocloc / libigdrcl /
          ;; libze_intel_gpu can find their siblings + IGC libs.
          (add-after 'install 'add-runtime-rpath
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (igc (assoc-ref inputs "intel-graphics-compiler"))
                     (out-lib (string-append out "/lib"))
                     (igc-lib (string-append igc "/lib")))
                (for-each
                 (lambda (f)
                   ;; `--add-rpath` appends to the existing RUNPATH
                   ;; without dropping the toolchain-provided entries
                   ;; Guix's `validate-runpath` already sees.
                   (invoke "patchelf" "--add-rpath" out-lib f)
                   (invoke "patchelf" "--add-rpath" igc-lib f))
                 (append (find-files (string-append out "/bin")
                                     ".*" #:stat lstat)
                         (find-files out-lib "\\.so.*$"
                                     #:stat lstat)))))))))
    (native-inputs (list cmake-minimal pkg-config patchelf))
    (inputs
     (list gmmlib
           intel-graphics-compiler
           level-zero
           libva))
    ;; OpenCL consumers (e.g. OpenVINO's intel_gpu_plugin, clinfo)
    ;; dlopen `libOpenCL.so.1` -- the ICD loader from ocl-icd -- which
    ;; then walks `OCL_ICD_VENDORS` (declared above) to find this
    ;; package's `intel.icd` and routes calls into `libigdrcl.so`.
    ;; Without the loader on the consumer's lib path the OpenVINO GPU
    ;; plugin's dlopen fails with "libOpenCL.so.1: cannot open shared
    ;; object file" even when intel.icd is present.  Propagating
    ;; ocl-icd ensures every profile carrying intel-compute-runtime
    ;; also carries the loader so the GPU plugin loads transparently.
    (propagated-inputs (list ocl-icd))
    (native-search-paths
     (list (search-path-specification
            (variable "OCL_ICD_VENDORS")
            (files '("etc/OpenCL/vendors")))))
    (home-page "https://github.com/intel/compute-runtime")
    (synopsis "Intel Compute Runtime (OpenCL + Level Zero GPU)")
    (description
     "The Intel Compute Runtime, also known as NEO, provides the
OpenCL ICD vendor driver (@code{libigdrcl.so}) and the Level Zero GPU
user-mode driver (@code{libze_intel_gpu.so}) for Intel integrated and
discrete GPUs.  Pair with @code{ocl-icd} for OpenCL applications and
@code{level-zero} for Level Zero programs; the latter is what
OpenVINO's intel_gpu plugin uses to dispatch GPU inference workloads
to the Intel iGPU on Tiger Lake, Alder Lake, Meteor Lake, Lunar Lake
and Panther Lake (and to discrete Arc / Battlemage cards).")
    (license license:expat)))
