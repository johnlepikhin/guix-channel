;; based on https://git.pantherx.org/development/panther/-/blob/master/px/packages/throttled.scm
(define-module (johnlepikhin packages throttled)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz))

(define-public throttled
  (package
    (name "throttled")
    (version "0.10.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/johnlepikhin/throttled/archive/refs/tags/v"
             version ".tar.gz"))
       (sha256
        (base32 "03mkwr6l98d9hb59b2spxacz8anyvklwvn45nrig85b5qvmr3pry"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils)
                                (srfi srfi-26))
                   (let* ((source (assoc-ref %build-inputs "source"))
                          (bash (assoc-ref %build-inputs "bash"))
                          (coreutils (assoc-ref %build-inputs "coreutils"))
                          (dbus (assoc-ref %build-inputs "dbus"))
                          (pciutils (assoc-ref %build-inputs "pciutils"))
                          (gzip (assoc-ref %build-inputs "gzip"))
                          (python (assoc-ref %build-inputs "python"))
                          (python-dbus (assoc-ref %build-inputs "python-dbus"))
                          (python-pygo (assoc-ref %build-inputs
                                                  "python-pygobject"))
                          (gobject-introspection-path (assoc-ref %build-inputs
                                                                 "gobject-introspection"))
                          (glib-path (assoc-ref %build-inputs "glib"))
                          (pythonpath (string-append "/lib/python"
                                                     ,(version-major+minor (package-version
                                                                            python))
                                                     "/site-packages:"))
                          (tar (assoc-ref %build-inputs "tar"))
                          (bin-dir (string-append %output "/bin"))
                          (etc-dir (string-append %output "/etc/throttled"))
                          (python-dir (string-append %output "/python"))
                          (root-dir (string-append "throttled-" ,version))
                          (bin-script (string-append root-dir
                                                     "/runit/throttled/run"))
                          (py-script (string-append root-dir "/throttled.py")))
                     (mkdir-p bin-dir)
                     (mkdir-p etc-dir)
                     (mkdir-p python-dir)
                     (setenv "PATH"
                             (string-append (string-append bash "/bin:")
                                            (string-append coreutils "/bin:")
                                            (string-append gzip "/bin:")
                                            (string-append tar "/bin:")))
                     (invoke "tar" "xvf" source)
                     (invoke "cp"
                             (string-append root-dir "/etc/throttled.conf")
                             (string-append etc-dir "/throttled.conf"))
                     (invoke "cp"
                             (string-append root-dir "/mmio.py") python-dir)
                     (substitute* py-script
                       (("/etc/throttled")
                        etc-dir))
                     (invoke "cp" py-script python-dir)
                     (substitute* bin-script
                       (("/opt/throttled/venv")
                        python))
                     (substitute* bin-script
                       (("/opt/throttled/throttled.py")
                        (string-append python-dir "/throttled.py" " $@")))
                     (invoke "mv" bin-script
                             (string-append bin-dir "/throttled"))
                     (wrap-program (string-append bin-dir "/throttled")
                       `("PATH" ":" prefix
                         (,(string-append pciutils "/sbin"))))
                     (wrap-program (string-append bin-dir "/throttled")
                       `("GI_TYPELIB_PATH" ":" prefix
                         (,(string-append glib-path "/lib/girepository-1.0"))))
                     (wrap-program (string-append bin-dir "/throttled")
                       `("PYTHONPATH" ":" prefix
                         (,(string-append python-dbus pythonpath))))
                     (wrap-program (string-append bin-dir "/throttled")
                       `("PYTHONPATH" ":" prefix
                         (,(string-append python-pygo pythonpath))))
                     (wrap-program (string-append bin-dir "/throttled")
                       `("PYTHONPATH" ":" prefix
                         (,(string-append python-pygo pythonpath))))

                     #t))))

    (native-inputs `(("coreutils" ,coreutils)
                     ("gzip" ,gzip)
                     ("python" ,python)
                     ("tar" ,tar)))
    (inputs
     `(("bash" ,bash)
       ("python-dbus" ,python-dbus)
       ("pciutils" ,pciutils)
       ("glib" ,glib)
       ("python-pygobject" ,python-pygobject)
       ("gobject-introspection" ,gobject-introspection)))
    (home-page "https://github.com/erpalma/throttled")
    (synopsis "Workaround for Intel throttling issues in Linux.")
    (description
     "The CPU package power limit (PL1/2) is forced to a value of 44 W (29 W on battery)
and the temperature trip point to 95 'C (85 'C on battery) by overriding default values
in MSR and MCHBAR every 5 seconds (30 on battery) to block the Embedded Controller from
resetting these values to default.")
    (license license:expat)))
