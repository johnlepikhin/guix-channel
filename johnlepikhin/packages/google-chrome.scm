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
  #:use-module (nongnu packages chrome))

(define-public google-chrome-beta-system-egl
  (package
    (inherit google-chrome-beta)
    (name "google-chrome-beta-system-egl")
    (inputs
     (modify-inputs (package-inputs google-chrome-beta)
                    (prepend mesa libva)))
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
                     ;; Set GPU environment variables
                     (format #t "export LIBGL_DRIVERS_PATH=\"~a:${LIBGL_DRIVERS_PATH}\"~%"
                             (string-append mesa "/lib/dri"))
                     (format #t "export MESA_LOADER_DRIVER_OVERRIDE=iris~%")
                     (format #t "export LIBVA_DRIVERS_PATH=\"~a:${LIBVA_DRIVERS_PATH}\"~%"
                             (string-append libva "/lib/dri"))
                     (format #t "export LIBVA_DRIVER_NAME=iHD~%")
                     (format #t "~%")
                     ;; Execute the original wrapper with GPU flags
                     (format #t "exec \"~a\" \\~%" (string-append wrapper "-original"))
                     (format #t "    --use-gl=angle \\~%")
                     (format #t "    --use-angle=gl \\~%")
                     (format #t "    --enable-features=VaapiVideoDecoder,VaapiVideoEncoder,CanvasOopRasterization,Vulkan \\~%")
                     (format #t "    --enable-gpu-rasterization \\~%")
                     (format #t "    --enable-zero-copy \\~%")
                     (format #t "    --disable-gpu-driver-bug-workarounds \\~%")
                     (format #t "    --ignore-gpu-blocklist \\~%")
                     (format #t "    \"$@\"~%")))
                 (chmod wrapper #o755)
                 #t)))))))))

google-chrome-beta-system-egl
