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

(define-module (johnlepikhin packages lightburn)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system copy)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages xorg)
  #:use-module ((gnu packages base) #:select (glibc))
  #:use-module ((nonguix licenses) #:prefix license:))

(define-public lightburn
  (package
    (name "lightburn")
    (version "1.7.08")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://release.lightburnsoftware.com/LightBurn/Release/"
                           "LightBurn-v" version "/LightBurn-Linux64-v" version ".7z"))
       (file-name (string-append "lightburn-" version ".7z"))
       (sha256
        (base32 "1n77kk88plwiwzn5zfld4gw0vxjj0wmji4k1x299qsnjvzgw0vvl"))))
    (build-system copy-build-system)
    (arguments
     (list
      ;; Prebuilt, self-contained Qt5 bundle: stripping and RUNPATH validation
      ;; make no sense here and only break the vendored libraries.
      #:strip-binaries? #f
      #:validate-runpath? #f
      #:install-plan
      #~'(("LightBurn" "share/lightburn"))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            (lambda* (#:key source #:allow-other-keys)
              (invoke "7z" "x" source)))
          (add-after 'install 'patch-elf
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((app (string-append #$output "/share/lightburn"))
                     (interpreter (string-append
                                   (assoc-ref inputs "glibc")
                                   #$(glibc-dynamic-linker)))
                     ;; Vendored libraries first: LightBurn ships its own Qt5
                     ;; and must not be mixed with the one from the store.
                     (rpath (string-join
                             (cons (string-append app "/lib")
                                   (map (lambda (label)
                                          (string-append (assoc-ref inputs label)
                                                         "/lib"))
                                        '("alsa-lib" "e2fsprogs"
                                          "fontconfig-minimal" "freetype"
                                          "gcc:lib" "glibc" "gmp"
                                          "libgpg-error" "libusb" "libx11"
                                          "libxcb" "mesa" "zlib")))
                             ":")))
                (for-each
                 (lambda (file)
                   (invoke "patchelf" "--set-rpath" rpath file))
                 (find-files app "\\.so($|\\.)"))
                (let ((binary (string-append app "/LightBurn")))
                  (invoke "patchelf" "--set-interpreter" interpreter
                          "--set-rpath" rpath binary)))))
          (add-after 'patch-elf 'install-launcher
            (lambda _
              (let ((bin (string-append #$output "/bin"))
                    (apps (string-append #$output "/share/applications"))
                    (icons (string-append #$output
                                          "/share/icons/hicolor/256x256/apps")))
                (mkdir-p bin)
                (symlink (string-append #$output "/share/lightburn/LightBurn")
                         (string-append bin "/lightburn"))
                (mkdir-p icons)
                (symlink (string-append #$output "/share/lightburn/LightBurn.png")
                         (string-append icons "/lightburn.png"))
                (mkdir-p apps)
                (call-with-output-file (string-append apps "/lightburn.desktop")
                  (lambda (port)
                    (format port "[Desktop Entry]~@
                                  Type=Application~@
                                  Name=LightBurn~@
                                  Comment=Control software for laser cutters~@
                                  Exec=~a/bin/lightburn %F~@
                                  Icon=lightburn~@
                                  Terminal=false~@
                                  Categories=Graphics;2DGraphics;Engineering;~@
                                  MimeType=application/x-lightburn;image/svg+xml;~%"
                            #$output))))))
          ;; AppRun is a symlink to the binary and is only meaningful inside
          ;; the original AppDir layout.
          (add-after 'install-launcher 'remove-apprun
            (lambda _
              (delete-file (string-append #$output "/share/lightburn/AppRun")))))))
    (native-inputs (list 7zip patchelf))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("e2fsprogs" ,e2fsprogs)
       ("fontconfig-minimal" ,fontconfig)
       ("freetype" ,freetype)
       ("gcc:lib" ,gcc "lib")
       ("glibc" ,glibc)
       ("gmp" ,gmp)
       ("libgpg-error" ,libgpg-error)
       ("libusb" ,libusb)
       ("libx11" ,libx11)
       ("libxcb" ,libxcb)
       ("mesa" ,mesa)
       ("zlib" ,zlib)))
    (supported-systems '("x86_64-linux"))
    (home-page "https://lightburnsoftware.com")
    (synopsis "Layout, editing and control software for laser cutters")
    (description
     "LightBurn is a proprietary application for laser cutters and engravers.
It lets you arrange, edit and create new vector shapes, apply per-layer cut
settings such as power, speed and number of passes, and send the resulting job
directly to the machine over USB or network.  A wide range of GRBL, Ruida,
Trocen and Marlin based controllers is supported.

Serial access to the machine requires membership in the @code{dialout} group.")
    (license (license:nonfree
              "https://lightburnsoftware.com/pages/eula"))))
