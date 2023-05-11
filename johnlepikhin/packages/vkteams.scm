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

(define-module (johnlepikhin packages vkteams)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
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
  #:export (make-vkteams))

;; Patched version, added support of xz
(define* (url-fetch/tarbomb url hash-algo hash
                            #:optional name
                            #:key (system (%current-system))
                            (guile (default-guile)))
  "Similar to 'url-fetch' but unpack the file from URL in a directory of its
own.  This helper makes it easier to deal with \"tar bombs\"."
  (define file-name
    (match url
      ((head k v)
       (basename head))
      (z
       (basename url))))
  (define gzip
    (module-ref (resolve-interface '(gnu packages compression)) 'gzip))
  (define tar
    (module-ref (resolve-interface '(gnu packages base)) 'tar))
  (define xz
    (module-ref (resolve-interface '(gnu packages compression)) 'xz))

  (mlet %store-monad ((drv (url-fetch url hash-algo hash
                                       (string-append "tarbomb-"
                                                      (or name file-name))
                                       #:system system
                                       #:guile guile))
                      (guile (package->derivation guile system)))
    ;; Take the tar bomb, and simply unpack it as a directory.
    ;; Use ungrafted tar/gzip so that the resulting tarball doesn't depend on
    ;; whether grafts are enabled.
    (gexp->derivation (or name file-name)
                      (with-imported-modules '((guix build utils))
                        #~(begin
                            (use-modules (guix build utils))
                            (mkdir #$output)
                            (setenv "PATH" (string-append #+gzip "/bin:" #+xz "/bin"))
                            (chdir #$output)
                            (invoke (string-append #+tar "/bin/tar")
                                    "xf" #$drv)))
                      #:system system
                      #:guile-for-build guile
                      #:graft? #f
                      #:local-build? #t)))


(define (make-vkteams version uri checksum)
  (package
    (name "vkteams")
    (version version)
    (source (origin
              (method url-fetch/tarbomb)
              (uri uri)
              (sha256 (base32 checksum))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       `(("vkteams" "dist/.vkteams-real")
         ("lib" "dist/lib")
         ("libexec" "dist/libexec")
         ("resources" "dist/resources")
         ("plugins" "dist/plugins"))
       #:phases
       (modify-phases
        %standard-phases
        (add-after
         'strip 'fix-binary
         (lambda*
          (#:key outputs inputs #:allow-other-keys)
          (let* ((out (assoc-ref outputs "out"))
                 (ncurses-lib
                  (string-append (assoc-ref inputs "ncurses") "/lib"))
                 (patchelf (string-append (assoc-ref inputs "patchelf") "/bin/patchelf"))
                 (binaries (string-append out "/dist/.vkteams-real"
                                          " " out "/dist/libexec/*"))
                 (libs (string-append out "/dist/lib/lib*.so*"))
                 (plugins (string-append out "/dist/plugins/*/lib*.so*"))
                 (dynamic-linker (string-append (assoc-ref inputs "libc") ,(glibc-dynamic-linker)))
                 (nss (string-append (assoc-ref inputs "nss") "/lib/nss")))
            (system
             (string-append
              patchelf
              " --set-rpath \"" out "/dist/lib:" nss ":$LIBRARY_PATH\""
              " --set-interpreter " dynamic-linker
              " " binaries))

            (system
             (string-append
              patchelf " --set-rpath \"" out "/dist/lib:" nss ":$LIBRARY_PATH\" " libs " " plugins))

            (mkdir (string-append out "/bin"))
            (let ((wrapper (string-append out "/bin/vkteams")))
              (with-output-to-file wrapper
                (lambda _
                  (display (string-append "#! /bin/sh\n\nSOFTWARE_RENDER=1 " out "/dist/.vkteams-real $*\n"))))
              (chmod wrapper #o755))

            (let ((qt.conf (string-append
                          "[Paths]\n"
                          "Plugins = " out "/dist/plugins\n"
                          "Libraries = " out "/dist/lib\n"
                          "LibraryExecutables = " out "/dist/libexec\n")))
              (with-output-to-file (string-append out "/dist/qt.conf")
                (lambda _
                  (display qt.conf))))

            (symlink
             (string-append ncurses-lib "/libncursesw.so."
                            ;; Extract "6.0" from "6.0-20170930" if a
                            ;; dash-separated version tag exists.
                            ,(let* ((v (package-version ncurses))
                                    (d (or (string-index v #\-)
                                           (string-length v))))
                               (version-major+minor (string-take v d))))
             (string-append out "/dist/lib/libtinfo.so.5"))

            #t)))
        (add-after
         'install 'finalize-install
         (lambda* (#:key outputs #:allow-other-keys)
           (let ((out (assoc-ref outputs "out")))
             (let ((apps (string-append out "/share/applications")))
               (mkdir-p apps)
               (make-desktop-entry-file
                (string-append apps "/vkteams.desktop")
                #:name "VK Teams"
                #:exec (string-append out "/bin/vkteams -urlcommand %U")
                #:mime-type (list
                             "x-scheme-handler/myteam-messenger"
                             "x-scheme-handler/vkteams-messenger"
                             "x-scheme-handler/vkteams"
                             "x-scheme-handler/callto"
                             "x-scheme-handler/zoomphonecall")
                #:categories '("Network" "Application")
                #:comment
                '(("en" "VK Teams messenger")
                  (#f "VK Teams messenger")))
               #t)))))))
    (synopsis "VK Teams")
    (description "VK Teams")
    (home-page "https://dl.internal.myteam.mail.ru/")
    (native-inputs `(("patchelf" ,patchelf)))
    (inputs `(("libxrandr" ,libxrandr)
              ("libxcomposite" ,libxcomposite)
              ("libxcursor" ,libxcursor)
              ("libxdamage" ,libxdamage)
              ("libgpg-error" ,libgpg-error)
              ("glibc" ,glibc)
              ("libfontconfig" ,fontconfig)
              ("libxau" ,libxau)
              ("pulseaudio" ,pulseaudio)
              ("dbus" ,dbus)
              ("mesa" ,mesa)
              ("glib" ,glib)
              ("libxi" ,libxi)
              ("eudev" ,eudev)
              ("ncurses" ,ncurses)
              ("libxtst" ,libxtst)
              ("alsa-lib" ,alsa-lib)
               ("nss" ,nss)
               ("libxinerama" ,libxinerama)))
    (license gpl3+)))

(define-public vkteams-23.02.0
  (make-vkteams "23.02.0" "https://t.bk.ru/lQ8MG7Buvx1ugDI/vkteams.tar.xz" "0kjng9rm89b8iz574p6ykvjvdhs8w5dn85da66mdsxkp9gkcka0x"))

(define-public vkteams vkteams-23.02.0)
