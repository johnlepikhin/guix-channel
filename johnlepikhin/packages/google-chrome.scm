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
  #:use-module (nongnu packages chrome))

(define-public google-chrome-beta-system-egl
  (package
    (inherit google-chrome-beta)
    (name "google-chrome-beta-system-egl")
    (inputs
     (modify-inputs (package-inputs google-chrome-beta)
                    (prepend mesa libva pciutils)))
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
                      (libva (assoc-ref inputs "libva")))
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
                     (format #t "export LIBVA_DRIVERS_PATH=\"~a:${LIBVA_DRIVERS_PATH}\"~%"
                             (string-append libva "/lib/dri"))
                     (format #t "export LIBVA_DRIVER_NAME=iHD~%")
                     (format #t "~%")
                     ;; Debug mode support
                     (format #t "# Debug mode support~%")
                     (format #t "CHROME_FLAGS=\"\"~%")
                     (format #t "if [ -n \"$CHROME_DEBUG\" ]; then~%")
                     (format #t "    CHROME_FLAGS=\"$CHROME_FLAGS --enable-logging=stderr --v=1\"~%")
                     (format #t "fi~%")
                     (format #t "~%")
                     ;; Execute the original wrapper with all flags
                     (format #t "# Execute Chrome with enhanced GPU and performance flags~%")
                     (format #t "exec \"~a\" \\~%" (string-append wrapper "-original"))
                     (format #t "    $CHROME_FLAGS \\~%")
                     ;; GPU flags
                     (format #t "    --use-gl=angle \\~%")
                     (format #t "    --use-angle=gl \\~%")
                     (format #t "    --enable-features=VaapiVideoDecoder,VaapiVideoEncoder,CanvasOopRasterization,Vulkan \\~%")
                     ;; Extended hardware decoding
                     (format #t "    --enable-features=VaapiVideoDecodeLinuxGL,VaapiAV1Decoder \\~%")
                     (format #t "    --enable-accelerated-video-decode \\~%")
                     ;; Performance optimization
                     (format #t "    --enable-features=ParallelDownloading,LazyFrameLoading,LazyImageLoading \\~%")
                     (format #t "    --max-active-webgl-contexts=16 \\~%")
                     (format #t "    --enable-quic \\~%")
                     (format #t "    --enable-tcp-fast-open \\~%")
                     ;; General GPU flags
                     (format #t "    --enable-gpu-rasterization \\~%")
                     (format #t "    --enable-zero-copy \\~%")
                     (format #t "    --disable-gpu-driver-bug-workarounds \\~%")
                     (format #t "    --ignore-gpu-blocklist \\~%")
                     ;; SSD optimization
                     (format #t "    --disk-cache-size=104857600 \\~%")
                     (format #t "    \"$@\"~%")))
                 (chmod wrapper #o755)
                 #t)))))))))

