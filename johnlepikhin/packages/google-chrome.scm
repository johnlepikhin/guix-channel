;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Evgenii Lepikhin <johnlepikhin@gmail.com>
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
;;;

;; Chrome uses ANGLE for OpenGL ES/WebGL support and expects specific GL implementations.
;; In Guix, Chrome fails to initialize GPU with error:
;; "Requested GL implementation (gl=none,angle=none) not found in allowed implementations: [(gl=egl-angle,angle=default)]"
;;
;; This happens because:
;; 1. Chrome's GPU process tries to initialize with wrong GL parameters
;; 2. The Mesa drivers path is not properly set in the environment
;; 3. VA-API drivers for hardware video decoding are not found
;;
;; This package fixes these issues by:
;; 1. Adding Mesa to inputs for proper OpenGL support
;; 2. Creating a new wrapper that sets environment variables and GPU flags

(define-module (johnlepikhin packages google-chrome)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix search-paths)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages video)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages commencement)
  #:use-module (nongnu packages chrome))

(define-public google-chrome-beta-system-egl
  (package
    (inherit google-chrome-beta)
    (name "google-chrome-beta-system-egl")
    (inputs
     (modify-inputs (package-inputs google-chrome-beta)
                    (prepend mesa mesa-utils libva pciutils gcc-toolchain)))
    (native-search-paths
     (append (package-native-search-paths google-chrome-beta)
             (list (search-path-specification
                    (variable "LIBGL_DRIVERS_PATH")
                    (files '("lib/dri")))
                   (search-path-specification
                    (variable "LIBVA_DRIVERS_PATH")
                    (files '("lib/dri")))
                   (search-path-specification
                    (variable "SSL_CERT_DIR")
                    (files '("etc/ssl/certs"))))))
    (arguments
     (substitute-keyword-arguments (package-arguments google-chrome-beta)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'install-wrapper 'add-gpu-wrapper
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (use-modules (ice-9 rdelim)
                            (ice-9 match))
               (let* ((out (assoc-ref outputs "out"))
                      (wrapper (string-append out "/bin/google-chrome-beta"))
                      (mesa (assoc-ref inputs "mesa"))
                      (mesa-utils (assoc-ref inputs "mesa-utils"))
                      (libva (assoc-ref inputs "libva"))
                      (gcc-toolchain (assoc-ref inputs "gcc-toolchain")))
                 ;; Move the original wrapper
                 (rename-file wrapper (string-append wrapper "-original"))
                 ;; Create new wrapper with GPU support
                 (with-output-to-file wrapper
                   (lambda ()
                     (format #t "#!/bin/sh~%")
                     (format #t "~%")
                     ;; Auto-detect GPU driver
                     (format #t "# Auto-detect GPU driver~%")
                     (format #t "if command -v lspci >/dev/null 2>&1; then~%")
                     (format #t "    if lspci | grep -qi nvidia; then~%")
                     (format #t "        export MESA_LOADER_DRIVER_OVERRIDE=nouveau~%")
                     (format #t "    elif lspci | grep -qi 'vga.*amd\\|vga.*advanced micro\\|vga.*radeon'; then~%")
                     (format #t "        export MESA_LOADER_DRIVER_OVERRIDE=radeonsi~%")
                     (format #t "    elif lspci | grep -qi 'vga.*intel'; then~%")
                     (format #t "        export MESA_LOADER_DRIVER_OVERRIDE=iris~%")
                     (format #t "    fi~%")
                     (format #t "else~%")
                     (format #t "    # Default to Intel if lspci is not available~%")
                     (format #t "    export MESA_LOADER_DRIVER_OVERRIDE=iris~%")
                     (format #t "fi~%")
                     (format #t "~%")
                     ;; Set GPU environment variables
                     (format #t "# GPU environment variables~%")
                     (format #t "export LIBGL_DRIVERS_PATH=\"~a:${LIBGL_DRIVERS_PATH}\"~%"
                             (string-append mesa "/lib/dri"))
                     ;; Use VA-API drivers from user's guix-home profile for version compatibility
                     (format #t "# VA-API will use drivers from user's guix-home profile~%")
                     (format #t "if [ -d \"$HOME/.guix-home/profile/lib/dri\" ]; then~%")
                     (format #t "    export LIBVA_DRIVERS_PATH=\"$HOME/.guix-home/profile/lib/dri:${LIBVA_DRIVERS_PATH}\"~%")
                     (format #t "else~%")
                     (format #t "    export LIBVA_DRIVERS_PATH=\"~a:${LIBVA_DRIVERS_PATH}\"~%"
                             (string-append libva "/lib/dri"))
                     (format #t "fi~%")
                     (format #t "export LIBVA_DRIVER_NAME=iHD~%")
                     (format #t "~%")
                     ;; Auto-detect OpenGL and GLSL versions for Mesa workaround
                     (format #t "# Auto-detect OpenGL and GLSL versions for Mesa workaround~%")
                     (format #t "# Using glxinfo from mesa-utils package~%")
                     (format #t "GLXINFO=\"~a/bin/glxinfo\"~%" mesa-utils)
                     (format #t "if [ -x \"$GLXINFO\" ]; then~%")
                     (format #t "    # Extract OpenGL version (e.g., \"4.6\" from \"4.6 (Compatibility Profile) Mesa 25.1.3\")~%")
                     (format #t "    GL_VERSION=$(\"$GLXINFO\" 2>/dev/null | grep \"OpenGL version string:\" | sed -E 's/OpenGL version string: ([0-9]+\\.[0-9]+).*/\\1/')~%")
                     (format #t "    # Extract GLSL version (e.g., \"460\" from \"4.60\")~%")
                     (format #t "    GLSL_VERSION=$(\"$GLXINFO\" 2>/dev/null | grep \"OpenGL shading language version string:\" | sed -E 's/OpenGL shading language version string: ([0-9]+)\\.([0-9]+).*/\\1\\2/')~%")
                     (format #t "    ~%")
                     (format #t "    if [ -n \"$GL_VERSION\" ] && [ -n \"$GLSL_VERSION\" ]; then~%")
                     (format #t "        export MESA_GL_VERSION_OVERRIDE=\"$GL_VERSION\"~%")
                     (format #t "        export MESA_GLSL_VERSION_OVERRIDE=\"$GLSL_VERSION\"~%")
                     (format #t "        if [ -n \"$CHROME_DEBUG\" ]; then~%")
                     (format #t "            echo \"Mesa GL version override: $GL_VERSION\" >&2~%")
                     (format #t "            echo \"Mesa GLSL version override: $GLSL_VERSION\" >&2~%")
                     (format #t "        fi~%")
                     (format #t "    fi~%")
                     (format #t "fi~%")
                     (format #t "~%")
                     ;; Debug mode support
                     (format #t "# Debug mode support~%")
                     (format #t "CHROME_FLAGS=\"\"~%")
                     (format #t "if [ -n \"$CHROME_DEBUG\" ]; then~%")
                     (format #t "    CHROME_FLAGS=\"$CHROME_FLAGS --enable-logging=stderr --v=1\"~%")
                     (format #t "fi~%")
                     (format #t "~%")
                     ;; Set LD_PRELOAD for libstdc++ compatibility
                     (format #t "# Preload newer libstdc++ for VA-API drivers~%")
                     (format #t "export LD_PRELOAD=\"~a/lib/libstdc++.so.6:${LD_PRELOAD}\"~%"
                             gcc-toolchain)
                     (format #t "~%")
                     ;; Execute the original wrapper with all flags
                     (format #t "# Execute Chrome with enhanced GPU and performance flags~%")
                     (format #t "exec \"~a\" \\~%" (string-append wrapper "-original"))
                     (format #t "    $CHROME_FLAGS \\~%")
                     ;; GPU flags
                     (format #t "    --use-gl=angle \\~%")
                     (format #t "    --use-angle=gl \\~%")
                     ;; Combine all features in one flag to avoid conflicts
                     (format #t "    --enable-features=VaapiVideoDecoder,VaapiVideoEncoder,VaapiVideoDecodeLinuxGL,VaapiAV1Decoder,VaapiIgnoreDriverChecks,CanvasOopRasterization,Vulkan,ParallelDownloading,LazyFrameLoading,LazyImageLoading \\~%")
                     (format #t "    --enable-accelerated-video-decode \\~%")
                     (format #t "    --enable-accelerated-video-encode \\~%")
                     (format #t "    --max-active-webgl-contexts=16 \\~%")
                     (format #t "    --enable-quic \\~%")
                     (format #t "    --enable-tcp-fast-open \\~%")
                     ;; General GPU flags
                     (format #t "    --enable-gpu-rasterization \\~%")
                     (format #t "    --enable-zero-copy \\~%")
                     (format #t "    --disable-gpu-driver-bug-workarounds \\~%")
                     (format #t "    --ignore-gpu-blocklist \\~%")
                     (format #t "    --disable-features=UseChromeOSDirectVideoDecoder \\~%")
                     ;; SSD optimization
                     (format #t "    --disk-cache-size=104857600 \\~%")
                     ;; Disable update notifications (Chrome cannot self-update in Guix)
                     (format #t "    --simulate-outdated-no-au=\"Tue, 31 Dec 2099 23:59:59 GMT\" \\~%")
                     (format #t "    \"$@\"~%")))
                 (chmod wrapper #o755)
                 #t)))))))))
