
(define-module (johnlepikhin packages oping)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages elf)
  #:use-module (guix build-system gnu))

(define (liboping version checksum)
  (package
   (name "liboping")
   (version version)
   (source (origin
            (method url-fetch)
            (uri (string-append "https://noping.cc/files/liboping-" version ".tar.bz2"))
            (sha256 (base32 checksum))))
   (build-system gnu-build-system)
   (supported-systems '("x86_64-linux"))
   (native-inputs `(("perl" ,perl)))
   (synopsis "octo's ping library")
   (description "octo's ping library")
   (home-page "https://noping.cc")
   (license lgpl2.1)))

(define-public liboping-1.10.0
  (liboping "1.10.0" "1n2wkmvw6n80ybdwkjq8ka43z2x8mvxq49byv61b52iyz69slf7b"))

(define-public liboping liboping-1.10.0)
