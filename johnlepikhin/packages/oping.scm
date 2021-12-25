
(define-module (johnlepikhin packages oping)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages elf)
  #:use-module (guix build-system gnu))

(define (liboping version commit checksum)
  (package
   (name "liboping")
   (version version)
   (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/johnlepikhin/liboping")
                    (commit commit)))
             (sha256 (base32 checksum))))
   (build-system gnu-build-system)
   (supported-systems '("x86_64-linux"))
   (native-inputs `(("perl" ,perl)))
   (synopsis "octo's ping library")
   (description "octo's ping library")
   (home-page "https://noping.cc")
   (license lgpl2.1)))

(define-public liboping-1.10.1
  (liboping "1.10.1" "7a6e4795c5fb1a98624c3a1c922da87b44476200" "1n2wkmvw6n80ybdwkjq8ka43z2x8mvxq49byv61b52iyz69slf7b"))

(define-public liboping liboping-1.10.1)
