;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Evgenii Lepikhin <johnlepikhin@gmail.com>
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

(define-module (johnlepikhin packages leftwm)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cargo)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix git)
  #:use-module (guix packages)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-vcs))

(define-public rust-lefthk-core-0.2
  (package
    (name "rust-lefthk-core")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "lefthk-core" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "0dwlvigc2lxswdr5vmxgq2j0ba6aqbl0cz5x0pg062qj5m45bplv"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/leftwm/lefthk")
    (synopsis "LeftHK - A hotkey daemon written in Rust")
    (description "LeftHK is a hotkey daemon written in Rust. It is a daemon that listens for hotkeys and executes commands.")
    (license (list license:expat license:asl2.0))))

(define-public rust-leftwm-core-0.5
  (package
    (name "rust-leftwm-core")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "leftwm-core" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "1kszdaq7pl517j7rh3f5acjwz95zpavwjp2xsq60ng2h6kywpkpy"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/leftwm/leftwm")
    (synopsis "A window manager for adventurers")
    (description "LeftWM is a window manager for adventurers. It is a tiling window manager that is easy to use and configure. It is written in Rust and is fully configurable.")
    (license (list license:expat license:asl2.0))))

(define-public rust-leftwm-layouts-0.8
  (package
    (name "rust-leftwm-layouts")
    (version "0.8.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "leftwm-layouts" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "0wyhnqfqdwwppx53iym4r3kdcvxial4sjmywlimigw2r7y6lplks"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/leftwm/leftwm")
    (synopsis "A window manager for adventurers")
    (description "LeftWM is a window manager for adventurers. It is a tiling window manager that is easy to use and configure. It is written in Rust and is fully configurable.")
    (license (list license:expat license:asl2.0))))

(define-public rust-leftwm-macros-0.5
  (package
    (name "rust-leftwm-macros")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "leftwm-macros" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "00bilkvl94q3rf93n3rv3vibsfnzr5qz7sym1c1aghi0nzygr7ry"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/leftwm/leftwm")
    (synopsis "A window manager for adventurers")
    (description "LeftWM is a window manager for adventurers. It is a tiling window manager that is easy to use and configure. It is written in Rust and is fully configurable.")
    (license (list license:expat license:asl2.0))))

(define-public rust-liquid-0.26
  (package
    (name "rust-liquid")
    (version "0.26.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "liquid" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "02dn20pg3a64hgjabx2mjwq8xir13y4k3xkrz0pax68l07hqmxk9"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/cobalt-org/liquid-rust")
    (synopsis "Liquid templating for Rust")
    (description "Liquid templating for Rust")
    (license (list license:expat license:asl2.0))))

(define-public rust-liquid-core-0.26
  (package
    (name "rust-liquid-core")
    (version "0.26.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "liquid-core" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "0psdy3vrcmlz2ykz8y2i67bvhw08r9wg382yjsvwzmdazi6p5q3r"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/cobalt-org/liquid-rust")
    (synopsis "Liquid templating for Rust")
    (description "Liquid templating for Rust")
    (license (list license:expat license:asl2.0))))

(define-public rust-liquid-derive-0.26
  (package
    (name "rust-liquid-derive")
    (version "0.26.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "liquid-derive" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "0887ccqm50ddj0d9n74ls6bm3m7p5mzdyjqm0cw7l9dlkcdb8bzw"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/cobalt-org/liquid-rust")
    (synopsis "Liquid templating for Rust")
    (description "Liquid templating for Rust")
    (license (list license:expat license:asl2.0))))

(define-public rust-liquid-lib-0.26
  (package
    (name "rust-liquid-lib")
    (version "0.26.4")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "liquid-lib" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "1h288fnpymix9w4536z1qy1ldgfhvxyqd9gpd1igpcbg78kpx8g2"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/cobalt-org/liquid-rust")
    (synopsis "Liquid templating for Rust")
    (description "Liquid templating for Rust")
    (license (list license:expat license:asl2.0))))

(define-public rust-anymap2-0.13
  (package
    (name "rust-anymap2")
    (version "0.13.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "anymap2" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "031kw3bp0zh2pn9fcayaw0w0gydgpgfhm08pg4yz5cml9jwv60fk"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/azriel91/anymap2")
    (synopsis "A safe and convenient store for one value of each type")
    (description "A safe and convenient store for one value of each type")
    (license (list license:expat license:asl2.0))))

(define-public rust-syslog-tracing-0.2
  (package
    (name "rust-syslog-tracing")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "syslog-tracing" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "0ghrjsj0psf0rd52j66dc02dk9yj2bsg3f5kfdryidqfyrl0p29g"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/max-heller/tracing-syslog")
    (synopsis "A tracing layer for syslog")
    (description "Support for logging tracing events to syslog through libc::syslog()")
    (license (list license:expat license:asl2.0))))

(define-public rust-tracing-appender-0.2
  (package
    (name "rust-tracing-appender")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tracing-appender" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "1kq69qyjvb4dxch5c9zgii6cqhy9nkk81z0r4pj3y2nc537fhrim"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/tokio-rs/tracing")
    (synopsis "Tracing for Rust")
    (description "tracing is a framework for instrumenting Rust programs to collect structured, event-based diagnostic information.
tracing is maintained by the Tokio project, but does not require the tokio runtime to be used.")
    (license (list license:expat license:asl2.0))))

(define-public rust-tracing-journald-0.3
  (package
    (name "rust-tracing-journald")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "tracing-journald" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "1zgkf2cx6m60qzgwsph83cbzma98b4vs5nshm2b3hg7wx1s6lcds"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/tokio-rs/tracing")
    (synopsis "Tracing for Rust")
    (description "tracing is a framework for instrumenting Rust programs to collect structured, event-based diagnostic information.
tracing is maintained by the Tokio project, but does not require the tokio runtime to be used.")
    (license (list license:expat license:asl2.0))))

(define-public rust-xlib-display-server-0.1
  (package
    (name "rust-xlib-display-server")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "xlib-display-server" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "1ikwgzmxcmz5q85d3pqly1n1k4zwlp8qr7if8sy6ni8hs0v8wc91"))))
    (build-system cargo-build-system)
    (home-page "https://docs.rs/xlib-display-server/0.1.2/xlib_display_server/")
    (synopsis "...")
    (description "...")
    (license (list license:expat license:asl2.0))))

(define-public rust-x11-dl-2
  (package
    (name "rust-x11-dl")
    (version "2.21.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "x11-dl" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "0vsiq62xpcfm0kn9zjw5c9iycvccxl22jya8wnk18lyxzqj5jwrq"))))
    (build-system cargo-build-system)
    (home-page "")
    (synopsis "...")
    (description "...")
    (license (list license:expat license:asl2.0))))

(define (make-leftwm version checksum)
  (package
    (name "leftwm")
    (version version)
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "leftwm" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 checksum))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #false
       #:cargo-build-flags
         '("--no-default-features", "--features", "sys-log,file-log,lefthk")
       #:cargo-inputs
       (("rust-anyhow-1" ,rust-anyhow-1)
        ("const_format" ,rust-const-format-0.2)
        ("rust-dirs-next" ,rust-dirs-next-2)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-git-version" ,rust-git-version-0.3)
        ("rust-lefthk-core" ,rust-lefthk-core-0.2)
        ("rust-leftwm-core" ,rust-leftwm-core-0.5)
        ("rust-leftwm-layouts" ,rust-leftwm-layouts-0.8)
        ("rust-leftwm-macros" ,rust-leftwm-macros-0.5)
        ("rust-liquid" ,rust-liquid-0.26)
        ("rust-liquid-core" ,rust-liquid-core-0.26)
        ("rust-liquid-derive" ,rust-liquid-derive-0.26)
        ("rust-liquid-lib" ,rust-liquid-lib-0.26)
        ("rust-mio" ,rust-mio-0.8)
        ("rust-nix" ,rust-nix-0.27)
        ("rust-ron" ,rust-ron-0.8)
        ("rust-shellexpand" ,rust-shellexpand-3)
        ("rust-signal-hook" ,rust-signal-hook-0.3)
        ("rust-syslog-tracing" ,rust-syslog-tracing-0.2)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-toml" ,rust-toml-0.8)
        ("rust-tracing-appender" ,rust-tracing-appender-0.2)
        ("rust-tracing-journald" ,rust-tracing-journald-0.3)
        ("rust-xlib-display-server" ,rust-xlib-display-server-0.1)
        ("rust-inventory" ,rust-inventory-0.3)
        ("rust-x11-dl" ,rust-x11-dl-2)
        ("rust-doc-comment" ,rust-doc-comment-0.3)
        ("rust-anymap2" ,rust-anymap2-0.13)
        ("rust-itertools" ,rust-itertools-0.10)
        ("rust-pest-derive" ,rust-pest-derive-2))))
    (home-page "https://github.com/leftwm/leftwm/")
    (synopsis "A window manager for adventurers")
    (description
     "LeftWM is a window manager for adventurers. It is a tiling window manager that is easy to use and configure. It is written in Rust and is fully configurable.")
    (license (list license:expat license:asl2.0))))

(define-public leftwm-0.5.1 (make-leftwm "0.5.1" "0h86d69j2axpgyxmcn4xfmadn564hnxvzi8lczab6srmyr6ckkif"))

(define-public leftwm leftwm-0.5.1)
