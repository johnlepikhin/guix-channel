(define-module (gnu packages linux-kernel usbio-linux-module)
  #:use-module (guix git-download)
  #:use-module (guix build-system linux-module)
  #:use-module (guix packages)
  #:use-module (nongnu packages linux)
  #:use-module ((guix licenses) #:prefix license:))

(define-public usbio-linux-module
  (package
    (name "usbio-linux-module")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/intel/usbio-drivers")
             (commit "4fb690c6d15a81c492954636c2db396cb700a119")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16yd6danqllxarqpklvs1iilbaaf0c0ly3zf38h9a64gv13k0nf7"))))
    (build-system linux-module-build-system)
    (arguments
     `(#:linux ,linux
       #:tests? #f))
    (home-page "https://github.com/intel/usbio-drivers")
    (synopsis "Linux kernel USBIO Extension drivers on Intel Alder Lake, Raptor Lake, Meteor Lake and Lunar Lake platforms")
    (description
     "Linux kernel USBIO Extension drivers on Intel Alder Lake, Raptor Lake, Meteor Lake and Lunar Lake platforms.")
    (license license:gpl2)))
