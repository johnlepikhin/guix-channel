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

;; Chrome is dynamically linked against libGLX and libEGL from the libglvnd library.
;; Libglvnd acts as a proxy/dispatcher that routes OpenGL API calls to the appropriate
;; backend provider (such as Mesa or proprietary drivers). It selects which implementation
;; to use at runtime based on available drivers.

;; However, in Guix, the version of libglvnd (or its integration with the rest of the stack)
;; may be outdated or incompatible, resulting in Chrome encountering `undefined symbol` errors
;; during library loading.

;; To work around this issue, I created a custom package that adds `mesa` to the runtime
;; dependencies. Crucially, `mesa` is prepended to the `inputs` list, so it appears first
;; in the generated LD_LIBRARY_PATH.

;; As a result, nonguix's build system (via the chromium-binary-build-system) generates a
;; wrapper script that sets LD_LIBRARY_PATH in the correct order. This ensures that Chrome
;; attempts to load libGLX and libEGL directly from Mesa first, avoiding conflicts or missing
;; symbols caused by relying solely on libglvnd.
(define-module (johnlepikhin packages google-chrome)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (gnu packages gl)
  #:use-module (nongnu packages chrome))

(define-public google-chrome-beta-system-egl
  (package
    (inherit google-chrome-beta)
    (name "google-chrome-beta-system-egl")
    (inputs
     (modify-inputs (package-inputs google-chrome-beta)
                    (prepend mesa)))))
