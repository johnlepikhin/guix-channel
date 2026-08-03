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
  #:use-module (johnlepikhin build rust)
  #:use-module (johnlepikhin packages rust-binary)
  #:use-module (johnlepikhin packages rust-crates))

(define-public ast-index
  (package
    (name "ast-index")
    (version "3.50.0")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/defendend/Claude-ast-index-search.git")
                   (commit "e499dcc6fcc90dfceafb629fbf5289824a40cccb")))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "19l9ywv24vcsakqx3f18dg7ds21lhjrzdssk067nn9lnvh59ws4f"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:rust rust-binary-1.88
      #:install-source? #f
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          #$%rust-cc-symlink-phase
          (add-after 'unpack 'remove-benchmarks
            (lambda _
              (when (file-exists? "benches")
                (delete-file-recursively "benches"))
              ;; Drop every [[bench]] table wholesale: the benchmarks pull in
              ;; criterion, and the set of them changes between releases, so
              ;; commenting out individual keys silently corrupts the file.
              ;; [patch.crates-io] goes too: it pins tree-sitter-scss to a git
              ;; revision for a Windows-only fix, and cargo cannot fetch git
              ;; sources in the offline build environment.
              (let* ((get-string-all (@ (ice-9 textual-ports) get-string-all))
                     (drop-table? (lambda (line)
                                    (or (string-prefix? "[[bench]]" line)
                                        (string-prefix? "[patch.crates-io]"
                                                        line))))
                     (lines (string-split
                             (call-with-input-file "Cargo.toml" get-string-all)
                             #\newline))
                     (kept (let loop ((rest lines) (drop? #f) (acc '()))
                             (cond
                              ((null? rest) (reverse acc))
                              ((drop-table? (car rest))
                               (loop (cdr rest) #t acc))
                              ((and drop? (string-prefix? "[" (car rest)))
                               (loop rest #f acc))
                              (drop? (loop (cdr rest) #t acc))
                              (else (loop (cdr rest) #f (cons (car rest) acc)))))))
                (call-with-output-file "Cargo.toml"
                  (lambda (port)
                    (display (string-join kept "\n") port)))))))))
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
