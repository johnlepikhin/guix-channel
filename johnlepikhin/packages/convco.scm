;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Evgenii Lepikhin <johnlepikhin@gmail.com>
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

(define-module (johnlepikhin packages convco)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-vcs)
  #:use-module (gnu packages crates-check)
  #:use-module (gnu packages crates-compression)
  #:use-module (gnu packages crates-web)
  #:use-module (gnu packages crates-windows)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module (guix packages))

(define-public rust-clap-builder-4.5.27
  (package
    (name "rust-clap-builder")
    (version "4.5.27")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "clap_builder" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1mys7v60lys8zkwpk49wif9qnja9zamm4dnrsbj40wdmni78h9hv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-anstream" ,rust-anstream-0.6)
                       ("rust-anstyle" ,rust-anstyle-1)
                       ("rust-clap-lex" ,rust-clap-lex-0.7)
                       ("rust-strsim" ,rust-strsim-0.11)
                       ("rust-terminal-size" ,rust-terminal-size-0.4))
       #:cargo-development-inputs (("rust-snapbox" ,rust-snapbox-0.6)
                                   ("rust-static-assertions" ,rust-static-assertions-1))))
    (home-page "https://github.com/clap-rs/clap")
    (synopsis "Simple to use, efficient, and full-featured Command Line Argument Parser")
    (description "This package provides a simple to use, efficient, and full-featured
Command Line Argument Parser.")
    (license (list license:expat license:asl2.0))))

(define-public rust-clap-derive-4.5.24
  (package
    (name "rust-clap-derive")
    (version "4.5.24")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "clap_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "131ih3dm76srkbpfx7zfspp9b556zgzj31wqhl0ji2b39lcmbdsl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-heck" ,rust-heck-0.5)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/clap-rs/clap")
    (synopsis "Parse command line argument by defining a struct, derive crate")
    (description "This package provides parse command line argument by defining
a struct, derive crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-clap-4.5.27
  (package
    (name "rust-clap")
    (version "4.5.27")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "clap" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "15j720q1z953h1qxm2q5nwkmyhhl2vb45v017rqlhjrbk12h36vn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-clap-builder" ,rust-clap-builder-4.5.27)
                       ("rust-clap-derive" ,rust-clap-derive-4.5.24))
       #:cargo-development-inputs (("rust-automod" ,rust-automod-1)
                                   ("rust-clap-cargo" ,rust-clap-cargo-0.14)
                                   ("rust-humantime" ,rust-humantime-2)
                                   ("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-shlex" ,rust-shlex-1)
                                   ("rust-snapbox" ,rust-snapbox-0.6)
                                   ("rust-trybuild" ,rust-trybuild-1)
                                   ("rust-trycmd" ,rust-trycmd-0.15))))
    (home-page "https://github.com/clap-rs/clap")
    (synopsis "Command Line Argument Parser")
    (description
     "This package provides a simple to use, efficient, and full-featured Command Line
Argument Parser.")
    (license (list license:expat license:asl2.0))))

(define-public rust-clap-complete-4.5.43
  (package
    (name "rust-clap-complete")
    (version "4.5.43")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "clap_complete" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0q8q8qsk3f6c1g5wk05i5836qp3rbdjh45j91ykdrin98lsh2lh9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-clap" ,rust-clap-4.5.27)
                       ("rust-clap-lex" ,rust-clap-lex-0.7)
                       ("rust-completest" ,rust-completest-0.4)
                       ("rust-completest-pty" ,rust-completest-pty-0.5)
                       ("rust-is-executable" ,rust-is-executable-1)
                       ("rust-shlex" ,rust-shlex-1))
       #:cargo-development-inputs (("rust-automod" ,rust-automod-1)
                                   ("rust-clap" ,rust-clap-4.5.27)
                                   ("rust-snapbox" ,rust-snapbox-0.6)
                                   ("rust-trycmd" ,rust-trycmd-0.15))))
    (home-page "https://github.com/clap-rs/clap")
    (synopsis "Generate shell completion scripts for your clap::Command")
    (description "This package provides shell completion scripts generation
for your clap::Command.")
    (license (list license:expat license:asl2.0))))

(define-public rust-num-modular-0.6
  (package
    (name "rust-num-modular")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "num-modular" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zv4miws3q1i93a0bd9wgc4njrr5j5786kr99hzxi9vgycdjdfqp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-num-bigint" ,rust-num-bigint-0.4)
                       ("rust-num-integer" ,rust-num-integer-0.1)
                       ("rust-num-traits" ,rust-num-traits-0.2))))
    (home-page "https://github.com/cmpute/num-modular")
    (synopsis "Efficient modular arithmetic operations")
    (description "This package provides efficient modular arithmetic operations.")
    (license (list license:expat license:asl2.0))))

(define-public rust-num-order-1
  (package
    (name "rust-num-order")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "num-order" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1dhvdncf91ljxh9sawnfxcbiqj1gnag08lyias0cy3y4jxmmjysk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-num-modular" ,rust-num-modular-0.6)
                       ("rust-num-traits" ,rust-num-traits-0.2))))
    (home-page "https://github.com/PyO3/rust-num-order")
    (synopsis "Compares two numbers, returns if they differ and how")
    (description "This package compares two numbers, returns if they differ and how.")
    (license license:expat)))

(define-public rust-handlebars-6
  (package
    (name "rust-handlebars")
    (version "6.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "handlebars" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1n8kp12ci4n6qydrbf5vkx3g3vmjcgamlckh0an6irn1jm5j4srx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-derive-builder" ,rust-derive-builder-0.20)
                       ("rust-heck" ,rust-heck-0.5)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-num-order" ,rust-num-order-1)
                       ("rust-pest" ,rust-pest-2)
                       ("rust-pest-derive" ,rust-pest-derive-2)
                       ("rust-rhai" ,rust-rhai-1)
                       ("rust-rust-embed" ,rust-rust-embed-8)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-thiserror" ,rust-thiserror-1)
                       ("rust-walkdir" ,rust-walkdir-2))
       #:cargo-development-inputs (("rust-criterion" ,rust-criterion-0.5)
                                   ("rust-env-logger" ,rust-env-logger-0.10)
                                   ("rust-pprof" ,rust-pprof-0.13)
                                   ("rust-serde-derive" ,rust-serde-derive-1)
                                   ("rust-tempfile" ,rust-tempfile-3)
                                   ("rust-time" ,rust-time-0.3)
                                   ("rust-tiny-http" ,rust-tiny-http-0.12))))
    (home-page "https://github.com/sunng87/handlebars-rust")
    (synopsis "Handlebars templating implemented in Rust")
    (description "Handlebars templating implemented in Rust.")
    (license license:expat)))

(define-public rust-jiff-tzdb-0.1.2
  (package
    (name "rust-jiff-tzdb")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "jiff-tzdb" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1lv0mb5ad182w5gkmb114h5v1ww9zfqlikhy0xdg8si6blpyqb6g"))))
    (build-system cargo-build-system)
    (arguments '(#:tests? #f))
    (home-page "https://github.com/BurntSushi/jiff")
    (synopsis "Embedded copy of the IANA Time Zone Database")
    (description "This package provides an embedded copy of the IANA Time Zone Database.")
    (license (list license:unlicense license:expat))))

(define-public rust-jiff-tzdb-platform-0.1.2
  (package
    (name "rust-jiff-tzdb-platform")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "jiff-tzdb-platform" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0zk9rb7b4xrdb3m1xlyhs4zziy57hpc548vrs9wjkfg70kj64g56"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-jiff-tzdb" ,rust-jiff-tzdb-0.1.2))))
    (home-page "https://github.com/BurntSushi/jiff")
    (synopsis "Acquire time zone configuration data")
    (description "This package provides a platform-specific crate for acquiring
time zone configuration data.")
    (license (list license:unlicense license:expat))))

(define-public rust-portable-atomic-1.10.0
  (package
    (name "rust-portable-atomic")
    (version "1.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "portable-atomic" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1rjfim62djiakf5rcq3r526hac0d1dd9hwa1jmiin7q7ad2c4398"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f))
    (home-page "https://github.com/taiki-e/portable-atomic")
    (synopsis "Portable atomic types")
    (description "This package provides portable atomic types.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-portable-atomic-util-0.2.4
  (package
    (name "rust-portable-atomic-util")
    (version "0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "portable-atomic-util" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "01rmx1li07ixsx3sqg2bxqrkzk7b5n8pibwwf2589ms0s3cg18nq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f
       #:cargo-inputs (("rust-portable-atomic" ,rust-portable-atomic-1.10.0))))
    (home-page "https://github.com/taiki-e/portable-atomic")
    (synopsis "Utilities for portable-atomic")
    (description "This package provides utilities for portable-atomic.")
    (license (list license:asl2.0 license:expat))))

(define-public rust-jiff-0.1.28
  (package
    (name "rust-jiff")
    (version "0.1.28")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "jiff" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "10as11fcrxa448lnzi6f3pi9mcax7957d8hiwv6zwr47w8lcf1y6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f
       #:cargo-inputs (("rust-jiff-tzdb" ,rust-jiff-tzdb-0.1.2)
                       ("rust-jiff-tzdb-platform" ,rust-jiff-tzdb-platform-0.1.2)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-portable-atomic" ,rust-portable-atomic-1.10.0)
                       ("rust-portable-atomic-util" ,rust-portable-atomic-util-0.2.4)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-windows-sys" ,rust-windows-sys-0.52))
       #:cargo-development-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-chrono-tz" ,rust-chrono-tz-0.9)
        ("rust-hifitime" ,rust-hifitime-3)
        ("rust-insta" ,rust-insta-1)
        ("rust-quickcheck" ,rust-quickcheck-1)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-tabwriter" ,rust-tabwriter-1)
        ("rust-time" ,rust-time-0.3)
        ("rust-tzfile" ,rust-tzfile-0.1)
        ("rust-walkdir" ,rust-walkdir-2))))
    (home-page "https://github.com/BurntSushi/jiff")
    (synopsis "Date-time library that provides high level datetime primitives")
    (description
     "This package provides a date-time library with high-level primitives that
are designed to be difficult to misuse and have reasonable performance.  It's
heavily inspired by the Temporal project.")
    (license (list license:unlicense license:expat))))

(define-public rust-semver-1.0.25
  (package
    (name "rust-semver")
    (version "1.0.25")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "semver" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "00sy306qpi7vfand7dxm2vc76nlc8fkh1rrhdy0qh12v50nzx7gp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-serde" ,rust-serde-1))))
    (home-page "https://github.com/dtolnay/semver")
    (synopsis "Semantic version parsing and comparison")
    (description
     "This package provides the parser and evaluator for Cargo's flavor of
Semantic Versioning.")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-derive-1.0.217
  (package
    (name "rust-serde-derive")
    (version "1.0.217")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "180r3rj5gi5s1m23q66cr5wlfgc5jrs6n1mdmql2njnhk37zg6ss"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://serde.rs")
    (synopsis "Macros 1.1 implementation of #[derive(Serialize, Deserialize)]")
    (description "This package provides macros 1.1 implementation of
#[derive(Serialize, Deserialize)].")
    (license (list license:expat license:asl2.0))))

(define-public rust-serde-1.0.217
  (package
    (name "rust-serde")
    (version "1.0.217")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0w2ck1p1ajmrv1cf51qf7igjn2nc51r0izzc00fzmmhkvxjl5z02"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs
       (("rust-serde-derive" ,rust-serde-derive-1.0.217))))
    (home-page "https://serde.rs")
    (synopsis "Generic serialization/deserialization framework")
    (description
     "This package provides a generic serialization/deserialization framework.")
    (license (list license:expat license:asl2.0))))

(define-public rust-unsafe-libyaml-norway-0.2
  (package
    (name "rust-unsafe-libyaml-norway")
    (version "0.2.13")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "unsafe-libyaml-norway" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0a8xn5az2fwlfxxc3kqys6fwy3k2nyla955691r8dj3wk94bc9b7"))))
    (build-system cargo-build-system)
    (arguments '(#:tests? #f))
    (home-page "https://github.com/cafkafk/unsafe-libyaml-norway")
    (synopsis "Rust bindings for libyaml")
    (description "This package provides Rust bindings for libyaml.")
    (license license:expat)))

(define-public rust-serde-norway-0.9.42
  (package
    (name "rust-serde-norway")
    (version "0.9.42")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_norway" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "130nx1r3nwydglq1yrrcydavd6w5zj219zsimc7m1zdmi6ag4274"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-indexmap" ,rust-indexmap-2)
                       ("rust-itoa" ,rust-itoa-1)
                       ("rust-ryu" ,rust-ryu-1)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-unsafe-libyaml-norway" ,rust-unsafe-libyaml-norway-0.2))
       #:cargo-development-inputs (("rust-anyhow" ,rust-anyhow-1)
                                   ("rust-indoc" ,rust-indoc-2)
                                   ("rust-serde-derive" ,rust-serde-derive-1))))
    (home-page "https://github.com/dtolnay/serde-yaml")
    (synopsis "YAML data format for Serde")
    (description "This package provides a YAML data format for Serde.")
    (license (list license:expat license:asl2.0))))

(define-public rust-thiserror-impl-2.0.11
  (package
    (name "rust-thiserror-impl")
    (version "2.0.11")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "thiserror-impl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1hkkn7p2y4cxbffcrprybkj0qy1rl1r6waxmxqvr764axaxc3br6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-2))))
    (home-page "https://github.com/dtolnay/thiserror")
    (synopsis "Implementation detail of the thiserror crate")
    (description "This package provides implementation detail of the
@code{thiserror} crate.")
    (license (list license:expat license:asl2.0))))

(define-public rust-thiserror-2.0.11
  (package
    (name "rust-thiserror")
    (version "2.0.11")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "thiserror" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1z0649rpa8c2smzx129bz4qvxmdihj30r2km6vfpcv9yny2g4lnl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-thiserror-impl" ,rust-thiserror-impl-2.0.11))
       #:cargo-development-inputs (("rust-anyhow" ,rust-anyhow-1)
                                   ("rust-ref-cast" ,rust-ref-cast-1)
                                   ("rust-rustversion" ,rust-rustversion-1)
                                   ("rust-trybuild" ,rust-trybuild-1))))
    (home-page "https://github.com/dtolnay/thiserror")
    (synopsis "Derive(Error) macro")
    (description "This package provides a @code{derive(Error)} macro for thiserror.")
    (license (list license:expat license:asl2.0))))

(define-public convco
  (package
    (name "convco")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri name version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1m8si96p1m60wg93syf3f77ksn0mg5cbd19n4n8hxn3n2ix9clhr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:install-source? #f
       ;; Disable default features which include zlib-ng
       #:cargo-build-flags '("--release" "--no-default-features")
       ;; Tests require zlib-ng, disable them
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-env-vars
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Ensure sys crates use system libraries
             (setenv "LIBZ_SYS_STATIC" "0")
             (setenv "LIBGIT2_NO_VENDOR" "1")
             (setenv "LIBSSH2_SYS_USE_PKG_CONFIG" "1")
             (setenv "LIBGIT2_SYS_USE_PKG_CONFIG" "1")
             (setenv "OPENSSL_NO_VENDOR" "1")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (invoke "cargo" "install" "--no-track"
                       "--path" "." "--root" out
                       "--no-default-features")))))
       #:cargo-inputs (("rust-clap" ,rust-clap-4.5.27)
                       ("rust-clap-complete" ,rust-clap-complete-4.5.43)
                       ("rust-dialoguer" ,rust-dialoguer-0.11)
                       ("rust-git2" ,rust-git2-0.20)
                       ("rust-handlebars" ,rust-handlebars-6)
                       ("rust-jiff" ,rust-jiff-0.1.28)
                       ("rust-itertools" ,rust-itertools-0.10)
                       ("rust-libz-sys" ,rust-libz-sys-1)
                       ("rust-percent-encoding" ,rust-percent-encoding-2)
                       ("rust-regex" ,rust-regex-1)
                       ("rust-semver" ,rust-semver-1.0.25)
                       ("rust-serde" ,rust-serde-1.0.217)
                       ("rust-serde-json" ,rust-serde-json-1)
                       ("rust-serde-norway" ,rust-serde-norway-0.9.42)
                       ("rust-thiserror" ,rust-thiserror-2.0.11)
                       ("rust-toml" ,rust-toml-0.5))
       #:cargo-development-inputs (("rust-assert-cmd" ,rust-assert-cmd-2)
                                   ("rust-duct" ,rust-duct-0.13)
                                   ("rust-tempfile" ,rust-tempfile-3))))
    (native-inputs (list pkg-config))
    (inputs (list git-minimal
                  zlib
                  libgit2-1.9
                  libssh2
                  openssl))
    (home-page "https://convco.github.io/")
    (synopsis "Conventional commit CLI")
    (description
     "Convco is a Conventional commit CLI tool written in Rust.  It provides tools
to work with Conventional Commits including commit message validation, changelog
generation, and version management.  The tool supports multiple shell completions
and can be used as a git core.editor.")
    (license license:expat)))
