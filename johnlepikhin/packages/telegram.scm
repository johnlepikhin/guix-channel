(define-module (johnlepikhin packages telegram)
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
