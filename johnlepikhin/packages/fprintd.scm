(define-module (johnlepikhin packages fprintd)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (guix build utils)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xdisorg)
  #:use-module (guix build-system meson)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public libfprint
  (package
    (name "libfprint")
    (version "1.94.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/libfprint/libfprint")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qgf4mf7x2rbj5xssagxrk3aa1h7dbs2l0lggjc25f37wmjzi6ix"))))
    (build-system meson-build-system)
    (arguments
     (list #:configure-flags
           #~(list "-Dudev_hwdb=disabled"
                   (string-append "-Dudev_hwdb_dir=" #$output
                                  "/lib/udev/hwdb.d")
                   (string-append "-Dc_link_args=-Wl,-rpath="
                                  (search-input-directory %build-inputs
                                                          "lib/nss"))
                   ;; (string-append "-Dc_link_args=-Wl,-rpath="
                   ;;                #$output "/lib")
                   (string-append "-Dudev_rules_dir=" #$output
                                  "/lib/udev/rules.d"))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'remove-tests
                 (lambda* (#:key outputs #:allow-other-keys)
                   (delete-file-recursively
                    (string-append (assoc-ref outputs "out")
                                   "/libexec/installed-tests")))))))
    (native-inputs
     (list `(,glib "bin")               ; for {glib-,}mkenums
           gobject-introspection
           gtk-doc/stable               ; for 88 KiB of API documentation
           pkg-config
           ;; For tests
           python-minimal))
    (inputs
     (list gusb
           libgudev
           nss                          ; for the URU4x00 driver
           ;; Replacing this with cairo works but just results in a reference
           ;; (only) to pixman in the end.
           pixman))
    (home-page "https://fprint.freedesktop.org/")
    (synopsis "Library to access fingerprint readers")
    (description
     "libfprint is a library designed to make it easy for application
developers to add support for consumer fingerprint readers to their
software.")
    (license license:lgpl2.1+)))

(define-public fprintd
  (package
    (name "fprintd")
    (version "1.94.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/libfprint/fprintd")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1r8mzivai8lc5aa880y37pjyzqfzci3hlap5s1vl8j33dxvkcs07"))))
    (build-system meson-build-system)
    (arguments
     (list #:configure-flags
           #~(list "-Dsystemd=false"
                   "-Dlibsystemd=libelogind"
                   (string-append "-Ddbus_service_dir=" #$output
                                  "/share/dbus-1/system-services")
                   (string-append "-Dpam_modules_dir=" #$output
                                  "/lib/security"))
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'configure 'patch-output-directories
                 ;; Install files to our output, not that of the ‘owner’ package.
                 ;; These are not exposed as Meson options and must be patched.
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((out (assoc-ref outputs "out")))
                     (substitute* "meson.build"
                       (("(dbus_interfaces_dir = ).*" _ set)
                        (string-append set "'" out "/share/dbus-1/interfaces'\n"))
                       (("(polkit_policy_directory = ).*" _ set)
                        (string-append set "'" out "/share/polkit-1/actions/'\n"))
                       (("(dbus_data_dir = ).*" _ set)
                        (string-append set "get_option('prefix')"
                                       " / get_option('datadir')\n"))))))
               (add-before 'configure 'patch-systemd-dependencies
                 (lambda _
                   (substitute* "meson.build"
                     (("(dependency\\(')(libsystemd|systemd)" _ prefix)
                      (string-append prefix "libelogind")))))
               (add-before 'configure 'ignore-test-dependencies
                 (lambda _
                   (substitute* "meson.build"
                     ((".*gi\\.repository\\.FPrint.*") "")
                     ;; (("pam_wrapper_dep .*") ""))))
           #:tests? #f))                    ; XXX depend on unpackaged packages
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")               ; for glib-genmarshal
           perl                         ; for pod2man
           pkg-config
           ;; For tests.
           python))                     ; needed unconditionally
    ;; pam_wrapper
    ;; python-pycairo
    ;; python-dbus
    ;; python-dbusmock
    ;; python-pygobject
    ;; python-pypamtest
    (inputs
     (list dbus-glib
           elogind
           libfprint
           linux-pam
           polkit
           cmake))
    (home-page "https://fprint.freedesktop.org/")
    (synopsis "D-Bus daemon that exposes fingerprint reader functionality")
    (description
     "fprintd is a D-Bus daemon that offers functionality of libfprint, a
library to access fingerprint readers, over the D-Bus interprocess
communication bus.  This daemon layer above libfprint solves problems related
to applications simultaneously competing for fingerprint readers.")
    (license license:gpl2+)))
