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

(define-module (johnlepikhin packages i3im)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages rust-crates)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module (guix git)
  #:use-module (guix packages)
  #:use-module (guix utils))

;; Local definitions to avoid dependency on crates-io modules
(define rust-env-logger-0.9
  (package
    (name "rust-env-logger")
    (version "0.9.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "env_logger" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1rq0kqpa8my6i1qcyhfqrn1g9xr5fbkwwbd42nqvlzn9qibncbm1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-log" ,rust-log-0.4)
                       ("rust-regex" ,rust-regex-1.11.1)
                       ("rust-atty" ,rust-atty-0.2)
                       ("rust-humantime" ,rust-humantime-2)
                       ("rust-termcolor" ,rust-termcolor-1))))
    (home-page "https://github.com/rust-cli/env_logger")
    (synopsis "Logging implementation for log")
    (description "This package provides a logging implementation for @code{log} which is configured via an environment variable.")
    (license (list license:expat license:asl2.0))))

(define rust-syslog
  (package
    (name "rust-syslog")
    (version "6.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "syslog" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0j295nplm4f6f7k3zcmbapwh61hi8x44ycwq5f3gyng0sywqjp7p"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/Geal/rust-syslog")
    (synopsis "Syslog message formatter and writer")
    (description "This package provides Syslog message formatter and writer, supporting UNIX sockets, UDP and TCP exporters.")
    (license license:expat)))

(define-public rust-i3ipc-jl-0
  (package
    (name "rust-i3ipc-jl")
    (version "0.11.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "i3ipc-jl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1lc4gyb2br8dy4crv3drn85pssmbykb9rwfpkzipglh74g0b97ql"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-byteorder" ,rust-byteorder-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-json" ,rust-serde-json-1))))
    (native-inputs (list pkg-config))
    (home-page "https://github.com/johnlepikhin/i3ipc-jl")
    (synopsis
     "A Rust library for controlling i3-wm through its IPC interface.")
    (description
     "A Rust library for controlling i3-wm through its IPC interface.")
    (license license:expat)))

(define-public rust-syslog-5
  (package
    (inherit rust-syslog)
    (name "rust-syslog")
    (version "5.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "syslog" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0paii62qnwjnfliygdal1x3hqxjkci1nlczfydv7kh3rnvqqwpcs"))))
    (arguments
     `(#:cargo-inputs (("rust-error-chain" ,rust-error-chain-0.12)
                       ("rust-hostname" ,rust-hostname-0.3)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-time" ,rust-time-0.3))))))

(define-public rust-slog-syslog-jl-0
  (package
    (name "rust-slog-syslog-jl")
    (version "0.13.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "slog-syslog-jl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "09hlvi4l1627fsmfxaclw0m7c2if40n7mmqrjrsxmbxgln0kb90r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-slog" ,rust-slog-2)
                       ("rust-syslog" ,rust-syslog-5))))
    (native-inputs (list pkg-config))
    (home-page "https://github.com/johnlepikhin/syslog-jl")
    (synopsis "Syslog drain for slog-rs")
    (description "Syslog drain for slog-rs")
    (license license:expat)))

(define-public rust-slog-envlogger-2
  (package
    (name "rust-slog-envlogger")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "slog-envlogger" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1h7m0jnj6kvsn9553fyqvaw3swy3pwpmwamqyhnnkv9zqh5ilslh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-log" ,rust-log-0.4)
                       ("rust-slog" ,rust-slog-2)
                       ("rust-slog-async" ,rust-slog-async-2)
                       ("rust-slog-scope" ,rust-slog-scope-4)
                       ("rust-slog-stdlog" ,rust-slog-stdlog-4)
                       ("rust-slog-term" ,rust-slog-term-2)
                       ("rust-regex" ,rust-regex-1.11.1))))
    (native-inputs (list pkg-config))
    (home-page "https://github.com/slog-rs/slog")
    (synopsis "Port of env_logger as a slog-rs drain")
    (description
     "env_logger is a de facto standard Rust logger implementation, which allows controlling logging to stderr via the
RUST_LOG environment variable.")
    (license license:expat)))

(define-public rust-structdoc-derive-0
  (package
    (name "rust-structdoc-derive")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "structdoc-derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1yjdi987jaqbypfanyllldk6ww2vswniniavn3pb4zrpazc75ah1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-either" ,rust-either-1)
                       ("rust-heck" ,rust-heck-0.3)
                       ("rust-itertools" ,rust-itertools-0.8)
                       ("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1)
                       ("rust-unindent" ,rust-unindent-0.1))))
    (native-inputs (list pkg-config))
    (home-page "https://github.com/vorner/structdoc")
    (synopsis "A procedural macro to generate documentation for structs")
    (description
     "A trait and procedural derive for extracting structure and documentation out of doc comments so they can be used at
runtime.")
    (license license:expat)))

(define-public rust-structdoc-0
  (package
    (name "rust-structdoc")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "structdoc" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "04bzjwlg8cxfbqgmg2i3s5y0lgmcsdj173byix2sa3dlf6955n4g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bitflags" ,rust-bitflags-1)
                       ("rust-itertools" ,rust-itertools-0.8)
                       ("rust-structdoc-derive" ,rust-structdoc-derive-0))))
    (native-inputs (list pkg-config))
    (home-page "https://github.com/vorner/structdoc")
    (synopsis "A procedural macro to generate documentation for structs")
    (description
     "A trait and procedural derive for extracting structure and documentation out of doc comments so they can be used at
runtime.")
    (license license:expat)))

(define-public i3im
  (package
    (name "i3im")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri name version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1h7a0jcma3fjqi8f4l0crdl67yqaypd0b1d6f81c84gjzm4j3rp3"))
       (modules '((guix build utils)))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-env-logger" ,rust-env-logger-0.9)
                       ("rust-structdoc" ,rust-structdoc-0)
                       ("rust-regex" ,rust-regex-1.11.1)
                       ("rust-anyhow" ,rust-anyhow-1)
                       ("rust-clap" ,rust-clap-4)
                       ("rust-i3ipc-jl" ,rust-i3ipc-jl-0)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-serde-regex" ,rust-serde-regex-1)
                       ("rust-serde-yaml" ,rust-serde-yaml-0.9)
                       ("rust-shellexpand" ,rust-shellexpand-3)
                       ("rust-slog" ,rust-slog-2)
                       ("rust-slog-envlogger" ,rust-slog-envlogger-2)
                       ("rust-slog-scope" ,rust-slog-scope-4)
                       ("rust-slog-stdlog" ,rust-slog-stdlog-4)
                       ("rust-slog-syslog-jl" ,rust-slog-syslog-jl-0)
                       ("rust-slog-term" ,rust-slog-term-2)
                       ("rust-serde-json" ,rust-serde-json-1))))
    (native-inputs (list pkg-config))
    (home-page "https://github.com/johnlepikhin/i3im")
    (synopsis "@command{i3im} i3 IMproved")
    (description
     "The utility extends the standard functionality of the i3 and Sway window managers.")
    (license (list license:expat))))
