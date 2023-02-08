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

(define-module (johnlepikhin packages zoom)
  #:use-module ((guix licenses) #:select (non-copyleft))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages video)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages kerberos)
  #:use-module (guix store)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy)
  #:export (make-zoom))

(define license:zoomus
  (non-copyleft "Proprietary Zoom.us license"
                "https://zoom.us/ru/terms.html"))

;; TODO broken
(define (make-zoom version checksum)
  (package
   (name "zoom")
   (version version)
   (source (origin
            (method url-fetch)
            (uri (string-append "https://cdn.zoom.us/prod/" version "/zoom_x86_64.tar.xz"))
            (sha256 (base32 checksum))))
   (build-system copy-build-system)
   (arguments
    `(#:install-plan
      ;; binaries
      `(("zoom" "zoom/bin/")
        ("zopen" "zoom/bin/")
        ("ZoomLauncher" "zoom/bin/")
        ("aomhost" "zoom/bin/")

        ;; directories
        ("calendar" "zoom/bin/")
        ("email" "zoom/bin/")
        ("json" "zoom/bin/")
        ("ringtone" "zoom/bin/")
        ("scheduler" "zoom/bin/")
        ("sip" "zoom/bin/")
        ("timezones" "zoom/bin/")
        ("translations" "zoom/bin/")
        ("cef" "zoom/bin/")
        ("Qt" "zoom/bin/")

        ;; libraries
        ("libfdkaac2.so" "zoom/bin/")
        ("libmkldnn.so" "zoom/bin/")
        ("libdvf.so" "zoom/bin/")
        ("libswresample.so.3" "zoom/bin/")
        ("libavformat.so.58" "zoom/bin/")
        ("libmpg123.so" "zoom/bin/")
        ("libavutil.so.56" "zoom/bin/")
        ("libavcodec.so.58" "zoom/bin/")
        ("libquazip.so" "zoom/bin/")
        ("libaomagent.so" "zoom/bin/")
        ("libOpenCL.so.1" "zoom/bin/")
        ("libclDNN64.so" "zoom/bin/")
        ("libturbojpeg.so" "zoom/bin/")

        ;; other files
        ("Beep-intercom.pcm" "zoom/bin/")
        ("clap-high.pcm" "zoom/bin/")
        ("clap-low.pcm" "zoom/bin/")
        ("clap-medium.pcm" "zoom/bin/")
        ("clap-very-low.pcm" "zoom/bin/")
        ("dingdong1.pcm" "zoom/bin/")
        ("dingdong.pcm" "zoom/bin/")
        ("double_beep.pcm" "zoom/bin/")
        ("Droplet.pcm" "zoom/bin/")
        ("Embedded.properties" "zoom/bin/")
        ("getbssid.sh" "zoom/bin/")
        ("leave.pcm" "zoom/bin/")
        ("meeting_chat_chime.pcm" "zoom/bin/")
        ("meeting_raisehand_chime.pcm" "zoom/bin/")
        ("qt.conf" "zoom/bin/")
        ("record_start.pcm" "zoom/bin/")
        ("record_stop.pcm" "zoom/bin/")
        ("ring.pcm" "zoom/bin/")
        ("version.txt" "zoom/bin/")
        ("wr_ding.pcm" "zoom/bin/")
        )
      #:phases
      (modify-phases
       %standard-phases
       (add-after
        'strip 'fix-binary
        (lambda*
            (#:key outputs inputs #:allow-other-keys)
          (let* ((out (assoc-ref outputs "out"))
                 (patchelf (string-append (assoc-ref inputs "patchelf") "/bin/patchelf"))
                 (binary (string-append
                          out "/zoom/bin/zoom"
                          " " out "/zoom/bin/zopen"
                          " " out "/zoom/bin/ZoomLauncher"
                          " " out "/zoom/bin/aomhost"))
                 (libs (string-append
                        out "/zoom/bin/lib*.so*"
                        " " out "/zoom/bin/*/lib*.so*"
                        " " out "/zoom/bin/*/*/lib*.so*"
                        " " out "/zoom/bin/*/*/*/lib*.so*"
                        " " out "/zoom/bin/*/*/*/*/lib*.so*"
                        ))
                 (dynamic-linker (string-append (assoc-ref inputs "libc") ,(glibc-dynamic-linker))))
            (system
             (string-append patchelf
                            " --set-rpath \""
                            out "/zoom/bin:"
                            out "/zoom/bin/cef:"
                            out "/zoom/bin/Qt/lib:"
                            "$LIBRARY_PATH\" --set-interpreter "
                            dynamic-linker
                            " "
                            binary))
            (system (string-append patchelf
                                   " --set-rpath \""
                                   out "/zoom/bin:"
                                   out "/zoom/bin/cef:"
                                   out "/zoom/bin/Qt/lib:"
                                   "$LIBRARY_PATH\" " libs))
            (system (string-append "rm -rf"
                                   " " out "/zoom/bin/Qt/qml/QtQuick/Scene3D"
                                   " " out "/zoom/bin/Qt/qml/QtQuick/Scene2D"
                                   " " out "/zoom/bin/Qt/qml/QtQuick/Particles.2"
                                   " " out "/zoom/bin/Qt/qml/QtQuick/LocalStorage"
                                   " " out "/zoom/bin/Qt/qml/QtQuick/XmlListModel"
                                   " " out "/zoom/bin/Qt/plugins/egldeviceintegrations"))
            (mkdir-p (string-append out "/bin"))
            (system (string-append "ln -s " out "/zoom/bin/zoom " out "/bin/zoom"))
            (system (string-append "ln -s " out "/zoom/bin/ZoomLauncher " out "/bin/ZoomLauncher"))
            #t)))
       (add-after
        'install 'finalize-install
        (lambda* (#:key outputs #:allow-other-keys)
          (let ((out (assoc-ref outputs "out")))
            (let ((apps (string-append out "/share/applications")))
              (mkdir-p apps)
              (make-desktop-entry-file
               (string-append apps "/zoom.desktop")
               #:name "Zoom"
               #:exec (string-append out "/zoom/bin/ZoomLauncher %U")
               #:mime-type (list
                            "x-scheme-handler/zoommtg"
                            "x-scheme-handler/zoomus"
                            "x-scheme-handler/tel"
                            "x-scheme-handler/callto"
                            "x-scheme-handler/zoomphonecall")
               #:categories '("Network" "Application")
               #:comment
               '(("en" "Zoom Video Conference")
                 (#f "Zoom Video Conference")))
              #t)))))))
   (synopsis "Zoom")
   (description "Zoom")
   (home-page "https://zoom.us")
   (native-inputs `(("patchelf" ,patchelf)))
   (inputs
    `(("alsa-lib" ,alsa-lib)
      ("atk" ,atk)
      ("cairo" ,cairo)
      ("dbus" ,dbus)
      ("fontconfig" ,fontconfig)
      ("freetype" ,freetype)
      ("gcc:lib" ,(canonical-package gcc) "lib")
      ("librsvg" ,librsvg)
      ("glib" ,glib)
      ("libnss" ,nss)
      ("libnspr4" ,nspr)
      ("libva" ,libva)
      ("gtk+" ,gtk+)
      ("libdrm" ,libdrm)
      ("libx11" ,libx11)
      ("libxcb" ,libxcb)
      ("libxcomposite" ,libxcomposite)
      ("libxext" ,libxext)
      ("libxfixes" ,libxfixes)
      ("libxkbcommon" ,libxkbcommon)
      ("libxrender" ,libxrender)
      ("libxtst" ,libxtst)
      ("libcups" ,cups)
      ("mit-krb5" ,mit-krb5)
      ("mesa" ,mesa)
      ("pango" ,pango)
      ("pulseaudio" ,pulseaudio)
      ("qtmultimedia" ,qtmultimedia)
      ("wayland" ,wayland)
      ("xcb-util-image" ,xcb-util-image)
      ("xcb-util-keysyms" ,xcb-util-keysyms)
      ("zlib" ,zlib)))
   (license license:zoomus)))

(define-public zoom-5.13.7.683 (make-zoom "5.13.7.683" "0rg6hbdaajfll1v7fjxlasrb65lhishmm7fz6c43dhznlh3xsy1f"))

(define-public zoom zoom-5.13.7.683)
