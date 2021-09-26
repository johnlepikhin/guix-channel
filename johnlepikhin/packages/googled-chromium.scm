(define-module (johnlepikhin packages chromium)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages video)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages speech)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages image)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages freedesktop)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:export (make-chromium google-chromium))

(define (make-chromium version version-uri checksum)
  (package
    (name "googled-chromium")
    (version version)
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append "https://commondatastorage.googleapis.com/chromium-browser-snapshots/Linux_x64/" version-uri "/chrome-linux.zip"))
              (sha256 (base32 checksum))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       `(("chrome-linux" "/")
         ("helper-script" "/bin/google-chromium"))
       #:phases
       (modify-phases
           %standard-phases
         (add-after 'unpack 'create-helper-script
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((font-config (string-append (assoc-ref inputs "fontconfig") "/etc/fonts/")))
               (with-output-to-file "helper-script"
                 (lambda _
                   (display
                    (string-append "#! /bin/sh\n\nFONTCONFIG_PATH=" font-config " $(dirname $(realpath $0))/../chrome-linux/chrome\n"))))
               #t)))
         (add-after
         'patch-shebangs 'fix-binary
         (lambda*
          (#:key outputs inputs #:allow-other-keys)
          (let* ((out (assoc-ref outputs "out"))
                 (patchelf (string-append (assoc-ref inputs "patchelf") "/bin/patchelf"))
                 (binaries (string-append out "/chrome-linux/{chrome,chrome_sandbox,chrome_crashpad_handler,nacl_helper}"))
                 (libs (string-append out "/chrome-linux/*.so* " out "/chrome-linux/swiftshader/*.so"))
                 (dynamic-linker (string-append (assoc-ref inputs "libc") ,(glibc-dynamic-linker)))
                 (nss (string-append (assoc-ref inputs "nss") "/lib/nss")))

            (system (string-append "chmod 755 " libs " " binaries " " out "/bin/google-chromium"))
            
            (system
             (string-append
              patchelf
              " --set-rpath \"$LIBRARY_PATH:" out "/chrome-linux:" nss "\""
              " --set-interpreter " dynamic-linker
              " " binaries))

            (system
             (string-append
              patchelf
              " --set-rpath \"$LIBRARY_PATH:" out "/chrome-linux:" nss "\" " libs))))))))
    (synopsis "Google Chromium")
    (description "Google Chromium")
    (home-page "https://www.google.com/intl/ru_ru/chrome/")
    (native-inputs `(("patchelf" ,patchelf)))
    (propagated-inputs
     `(("alsa-lib" ,alsa-lib)
       ("at-spi2-atk" ,at-spi2-atk)
       ("at-spi2-core" ,at-spi2-core)
       ("atk" ,atk)
       ("cairo" ,cairo)
       ("cups" ,cups)
       ("curl" ,curl)
       ("dbus" ,dbus)
       ("expat" ,expat)
       ("ffmpeg" ,ffmpeg)
       ("flac" ,flac)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("gcc:lib" ,(canonical-package gcc) "lib")
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("glib" ,glib)
       ("glibc" ,glibc)
       ("gtk+" ,gtk+)
       ("harfbuzz" ,harfbuzz)
       ("lcms" ,lcms)
       ("libdrm" ,libdrm)
       ("libevent" ,libevent)
       ("libffi" ,libffi)
       ("libjpeg-turbo" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libva" ,libva)
       ("libwebp" ,libwebp)
       ("libx11" ,libx11)
       ("libxcb" ,libxcb)
       ("libxcomposite" ,libxcomposite)
       ("libxcursor" ,libxcursor)
       ("libxdamage" ,libxdamage)
       ("libxext" ,libxext)
       ("libxfixes" ,libxfixes)
       ("libxi" ,libxi)
       ("libxkbcommon" ,libxkbcommon)
       ("libxml2" ,libxml2)
       ("libxrandr" ,libxrandr)
       ("libxrender" ,libxrender)
       ("libxscrnsaver" ,libxscrnsaver)
       ("libxshmfence" ,libxshmfence)
       ("libxslt" ,libxslt)
       ("libxtst" ,libxtst)
       ("mesa" ,mesa)
       ("minizip" ,minizip)
       ("mit-krb5" ,mit-krb5)
       ("nss" ,nss)
       ("openh264" ,openh264)
       ("openjpeg" ,openjpeg)                          ;PDFium only
       ("pango" ,pango)
       ("pciutils" ,pciutils)
       ("pipewire" ,pipewire-0.3)
       ("pulseaudio" ,pulseaudio)
       ("snappy" ,snappy)
       ("speech-dispatcher" ,speech-dispatcher)
       ("udev" ,eudev)
       ("vulkan-headers" ,vulkan-headers)
       ("wayland" ,wayland)
       ("xdg-utils" ,xdg-utils)))
    (license (list license:bsd-3
                   license:bsd-2
                   license:expat
                   license:asl2.0
                   license:mpl1.1
                   license:mpl2.0
                   license:public-domain
                   license:isc
                   (license:non-copyleft "chrome://credits"
                                         "See chrome://credits for more information.")
                   license:lgpl2.1+))))

(define-public google-chromium-92.5087
  (make-chromium "92.5087.rev9" "925087" "0bwbkvxcp3j0k15ns0kfmhknm6rrbhjbmv3xrnpwsqbp68abssdg"))

(define-public google-chromium google-chromium-92.5087)
