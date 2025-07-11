;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Evgenii Lepikhin <johnlepikhin@gmail.com>
;;;
;;; This file is part of GNU Guix.
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

(define-module (johnlepikhin packages crates-io)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages python)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages base)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages version-control)
  #:use-module (johnlepikhin packages rust-binary)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-web)
  #:use-module (gnu packages crates-crypto)
  #:use-module (gnu packages crates-apple)
  #:use-module (gnu packages crates-windows))


(define-public rust-docs-mcp-server
  (package
    (name "rust-docs-mcp-server")
    (version "1.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Govcraft/rust-docs-mcp-server")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1af1wz4rkqwdkbib1zs7skgz988fhghla4gsc5prdda4lslbh9ld"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-rmcp" ,rust-rmcp-0.1)
        ("rust-tokio" ,rust-tokio-1)
        ("rust-dotenvy" ,rust-dotenvy-0.15)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-thiserror" ,rust-thiserror-2)
        ("rust-walkdir" ,rust-walkdir-2)
        ("rust-scraper" ,rust-scraper-0.23)
        ("rust-ndarray" ,rust-ndarray-0.16)
        ("rust-async-openai" ,rust-async-openai-0.28)
        ("rust-futures" ,rust-futures-0.3)
        ("rust-bincode" ,rust-bincode-2)
        ("rust-tiktoken-rs" ,rust-tiktoken-rs-0.6)
        ("rust-cargo" ,rust-cargo-0.87)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-anyhow" ,rust-anyhow-1)
        ("rust-schemars" ,rust-schemars-0.8)
        ("rust-clap" ,rust-clap-4)
        ("rust-xdg" ,rust-xdg-2)
        ("rust-dirs" ,rust-dirs-6))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("openssl" ,openssl)))
    (home-page "https://github.com/Govcraft/rust-docs-mcp-server")
    (synopsis "MCP server for accurate Rust documentation context")
    (description "This MCP (Model Context Protocol) server prevents outdated Rust code
suggestions from AI assistants by fetching current crate documentation.  It uses
embeddings and LLMs to provide accurate context via tool calls.  The server requires
an OpenAI API key for embeddings and summarization.")
    (license license:expat)))
