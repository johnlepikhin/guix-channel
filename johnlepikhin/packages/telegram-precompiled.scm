;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Evgenii Lepikhin <johnlepikhin@gmail.com>
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

(define-module (johnlepikhin packages telegram-precompiled)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix licenses)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:export (make-telegram-desktop-precompiled
            telegram-desktop-precompiled))

(define (make-telegram-desktop-precompiled version checksum)
  (package
    (name "telegram-desktop-precompiled")
    (version version)
    (source (origin
              (method url-fetch)
              (uri (string-append "https://updates.tdesktop.com/tlinux/tsetup." version ".tar.xz"))
              (sha256 (base32 checksum))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       `(("Telegram" "bin/Telegram"))
       #:phases
       (modify-phases
        %standard-phases
        (add-after
         'strip 'fix-binary
         (lambda*
          (#:key outputs inputs #:allow-other-keys)
          (let* ((out (assoc-ref outputs "out"))
                 (patchelf (string-append (assoc-ref inputs "patchelf") "/bin/patchelf"))
                 (dynamic-linker (string-append (assoc-ref inputs "libc") ,(glibc-dynamic-linker)))
                 (nss (string-append (assoc-ref inputs "nss") "/lib/nss")))
            (system
             (string-append
              patchelf
              " --set-rpath \"" nss ":$LIBRARY_PATH\""
              " --set-interpreter " dynamic-linker
              " " out "/bin/Telegram"))

            #t))))))
    (synopsis "Telegram desktop precompiled binary")
    (description "Package provides latest versions of Telegram desktop by patching precompiled official binaries")
    (home-page "https://desktop.telegram.org/")
    (native-inputs `(("patchelf" ,patchelf)))
    (inputs `(
              ("libxrandr" ,libxrandr)
              ("glibc" ,glibc)
              ("libfontconfig" ,fontconfig)
              ("pulseaudio" ,pulseaudio)
              ("dbus" ,dbus)
              ("mesa" ,mesa)
              ("libx11" ,libx11)
              ("libxcb" ,libxcb)
              ("glib" ,glib)
              ("gtk+" ,gtk+)
              ("alsa-lib" ,alsa-lib)
              ("nss" ,nss)))
    (license gpl3+)))

(define-public telegram-desktop-precompiled-4.0.2
  (make-telegram-desktop-precompiled "4.0.2" "1706nw5pxl1cabdcbfksqqm159y62mrvhnfr9k0gg1dd9iiilc99"))

(define-public telegram-desktop-precompiled-4.1.0
  (make-telegram-desktop-precompiled "4.1.0" "0xkvfd3n8k2v93v8h32hl79ynd84ygc6zwbr3cps1ihakzh4rxk0"))

(define-public telegram-desktop-precompiled telegram-desktop-precompiled-4.1.0)
