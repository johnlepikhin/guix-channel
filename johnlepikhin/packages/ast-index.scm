;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Evgenii Lepikhin <johnlepikhin@gmail.com>
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

(define-module (johnlepikhin packages ast-index)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages sqlite)
  #:use-module (guix build-system cargo)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (johnlepikhin packages rust-binary)
  #:use-module (johnlepikhin packages rust-crates))

(define-public ast-index
  (package
    (name "ast-index")
    (version "3.24.0")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/defendend/Claude-ast-index-search.git")
                   (commit "fceacdd8d41b7e07392895077cfbc0702684b450")))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "13hqmn3da3p0k00wv9swvzrvmzadcz25hpynz66px08p4iad64l0"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:rust rust-binary-1.88
      #:install-source? #f
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'create-cc-symlink
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((gcc (assoc-ref inputs "gcc-toolchain")))
                (mkdir-p "/tmp/bin")
                (symlink (string-append gcc "/bin/gcc") "/tmp/bin/cc")
                (setenv "PATH" (string-append "/tmp/bin:" (getenv "PATH")))
                (setenv "CC" (string-append gcc "/bin/gcc"))
                (setenv "HOST_CC" (string-append gcc "/bin/gcc")))))
          (add-after 'unpack 'remove-benchmarks
            (lambda _
              (when (file-exists? "benches")
                (delete-file-recursively "benches"))
              (substitute* "Cargo.toml"
                (("\\[\\[bench\\]\\]") "# [[bench]]")
                (("name = \"parser_bench\"") "# name = \"parser_bench\"")
                (("name = \"db_bench\"") "# name = \"db_bench\"")
                (("name = \"pipeline_bench\"") "# name = \"pipeline_bench\"")
                (("harness = false") "# harness = false")))))))
    (native-inputs (list gcc-toolchain pkg-config))
    (inputs (append (list sqlite)
                    (cargo-inputs 'ast-index
                                  #:module '(johnlepikhin packages rust-crates))))
    (home-page "https://github.com/defendend/Claude-ast-index-search")
    (synopsis "Fast AST-based code search CLI for 23 programming languages")
    (description "ast-index is a fast code search CLI that indexes source code
using tree-sitter AST parsing and SQLite FTS5 full-text search.  It supports
23 programming languages including Rust, Python, Go, Java, Kotlin, Swift,
TypeScript, C++, and more.  Designed for AI agents and developers using Claude
or Cursor for rapid code discovery.")
    (license license:expat)))
