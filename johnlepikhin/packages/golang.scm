;; Based on the gopls package in Guix, but with the version number changed
;; and compiled with Go 1.20.
(define-module (johnlepikhin packages golang)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system go)
  #:use-module (guix packages)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages golang-check))

(define-public go-golang-org-x-telemetry
  (package
    (name "go-golang-org-x-telemetry")
    (version "0.10.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://go.googlesource.com/telemetry")
                    (commit (string-append "config/v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "16d63fqpcnnl701n4gl0p5lnkxq49ibmidfvznk1ib1z60p1x35a"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "golang.org/x/telemetry"
       ;; Source-only package
       #:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://golang.org/x/telemetry")
    (synopsis "Telemetry server code and libraries for Go")
    (description
     "This repository holds the Go Telemetry server code and libraries.")
    (license license:bsd-3)))

(define-public go-golang-org-x-vuln-1
  (package
    (name "go-golang-org-x-vuln-1")
    (version "1.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://go.googlesource.com/vuln")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0m0fjj544b8fxv5jdg52hw9jx2sgv8hfzzigm7gsn0gjw2p4j1n0"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "golang.org/x/vuln"
       ;; Source-only package
       #:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build))))
    (home-page "https://golang.org/x/vuln")
    (synopsis "Go vulnerability scanner")
    (description
     "Package scan provides functionality for running govulncheck.")
    (license license:bsd-3)))

(define-public gopls-go1.20
  (package
    (name "gopls-go1.20")
    (version "0.14.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://go.googlesource.com/tools")
                    (commit (string-append "gopls/v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0qyqrjdrhdcz5a5c8iyyvrrwidxh9nn23z18v4b6vshsk4irrj45"))))
    (build-system go-build-system)
    (arguments
     `(#:go ,go-1.20
       #:import-path "golang.org/x/tools/gopls"
       #:unpack-path "golang.org/x/tools"
       #:install-source? #f
       #:phases (modify-phases %standard-phases
                  (add-before 'unpack 'override-tools
                    (lambda _
                      (delete-file-recursively "src/golang.org/x/tools"))))))
    (propagated-inputs (list go-github-com-google-go-cmp-cmp
                             go-github-com-sergi-go-diff
                             go-golang-org-x-sys
                             go-golang-org-x-telemetry
                             go-golang-org-x-text
                             go-honnef-co-go-tools
                             go-mvdan-cc-gofumpt
                             go-golang-org-x-vuln-1
                             go-mvdan-cc-xurls))
    (home-page "https://golang.org/x/tools/gopls")
    (synopsis "Official language server for the Go language")
    (description
     "Pronounced ``Go please'', this is the official Go language server
developed by the Go team.  It provides IDE features to any LSP-compatible
editor.")
    (license license:bsd-3)))
