;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Evgenii Lepikhin <johnlepikhin@gmail.com>
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

(define-module (johnlepikhin packages mikrotik-winbox)
  #:use-module ((guix licenses) #:select (non-copyleft))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (guix store)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy))

(define license:mikrotik
  (non-copyleft "Proprietary Mikrotik license"
                "https://mikrotik.com/downloadterms.html"))

(define (make-winbox version version-string checksum)
  (package
   (name "mikrotik-winbox")
   (version version)
   (source (origin
             (method url-fetch)
             (uri (string-append "https://download.mikrotik.com/routeros/winbox/" version-string "/WinBox_Linux.zip"))
             (sha256 (base32 checksum))))
   (build-system copy-build-system)
   (arguments
    `(#:install-plan
      `(("../WinBox" "winbox/bin/")
        ("../assets" "winbox/bin/"))
      #:phases
      (modify-phases
       %standard-phases
       (add-after
        'strip 'fix-binary
        (lambda*
            (#:key outputs inputs #:allow-other-keys)
          (let* ((out (assoc-ref outputs "out"))
                 (patchelf (string-append (assoc-ref inputs "patchelf") "/bin/patchelf"))
                 (binary (string-append out "/winbox/bin/WinBox"))
                 (dynamic-linker (string-append (assoc-ref inputs "libc") ,(glibc-dynamic-linker))))
            (system
             (string-append patchelf " --set-rpath \"$LIBRARY_PATH\" --set-interpreter " dynamic-linker " " binary))
            (mkdir-p (string-append out "/bin"))
            (system (string-append "ln -s " out "/winbox/bin/WinBox " out "/bin/winbox"))
            #t))))))
   (synopsis "Mikrotik Winbox client for Linux")
   (description "Mikrotik Winbox is a utility for managing Mikrotik RouterOS devices.")
   (home-page "https://mikrotik.com")
   (native-inputs `(("patchelf" ,patchelf)))
   (inputs
    `(("unzip" ,unzip)
      ("fontconfig" ,fontconfig)
      ("freetype" ,freetype)
      ("libx11" ,libx11)
      ("libxcb" ,libxcb)
      ("mesa" ,mesa)
      ("xcb-util-image" ,xcb-util-image)
      ("xcb-util-keysyms" ,xcb-util-keysyms)
      ("xcb-util-wm" ,xcb-util-wm)
      ("xcb-util-renderutil" ,xcb-util-renderutil)
      ("libxkbcommon" ,libxkbcommon)
      ("zlib" ,zlib)))
   (license license:mikrotik)))

(define-public mikrotik-winbox-4.0beta21 (make-winbox "4.0" "4.0beta21" "1xsb62hdcrfvmfw643clbgapg4as7mg4hcaa73ar5m4nw37v11jj"))

(define-public mikrotik-winbox mikrotik-winbox-4.0beta21)
