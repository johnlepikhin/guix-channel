
(define-module (johnlepikhin packages oping)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (guix packages)
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
   (native-inputs `(("perl" ,perl)
                    ("autoconf" ,autoconf)
                    ("libtool" ,libtool)
                    ("pkg-config" ,pkg-config)
                    ("automake" ,automake)))
   (synopsis "octo's ping library")
   (description "octo's ping library")
   (home-page "https://noping.cc")
   (license lgpl2.1)))

(define-public liboping-1.10.1
  (liboping "1.10.1" "7a6e4795c5fb1a98624c3a1c922da87b44476200" "1nmhi4652izd0adrs72m038cy035snn7c8kmhrafx2zw7vwwnrgj"))

(define-public liboping liboping-1.10.1)

liboping
