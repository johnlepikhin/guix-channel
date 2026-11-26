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

(define-module (johnlepikhin packages gemini-api-cli)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix build-system go)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz))

(define-public go-github-com-alpkeskin-gotoon
  (package
    (name "go-github-com-alpkeskin-gotoon")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/alpkeskin/gotoon")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1c3r8jhy1wlfax33qh7nhgyyqj29pl3fw9677yppz6s9g02hwbmc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/alpkeskin/gotoon"
      ;; The examples are standalone `main' packages.
      #:test-subdirs #~(list ".")))
    (home-page "https://github.com/alpkeskin/gotoon")
    (synopsis "Token-Oriented Object Notation encoder for Go")
    (description
     "This package encodes Go values as TOON (Token-Oriented Object
Notation), a compact, human-readable serialization of the JSON data model
meant for passing structured data to large language models with fewer
tokens than JSON takes.")
    (license license:expat)))

(define-public go-github-com-spyzhov-ajson
  (package
    (name "go-github-com-spyzhov-ajson")
    (version "0.9.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/spyzhov/ajson")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0id4lyz0xpp6qf1sp5z6383wm6rckphvf34z1jg7lspwbavirdp3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/spyzhov/ajson"))
    (home-page "https://github.com/spyzhov/ajson")
    (synopsis "Abstract JSON for Go with JSONPath support")
    (description
     "This package parses JSON into a tree of nodes without binding it to
Go types.  The tree can be walked, modified, evaluated and queried with
JSONPath expressions, and each node is unmarshalled only on demand.")
    (license license:expat)))

(define-public gemini-api-cli
  (package
    (name "gemini-api-cli")
    (version "0.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/google-gemini/gemini-api-cli")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "172h30y9sixz9pjixljjslm2icds1gsrsksq885711b5325ldv6m"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.26
      #:install-source? #f
      #:import-path "github.com/google-gemini/gemini-api-cli/cmd/gemini-api"
      #:unpack-path "github.com/google-gemini/gemini-api-cli"
      #:phases
      #~(modify-phases %standard-phases
          ;; The tests live outside of the import path, which only holds
          ;; the `main' package.  `docs/self-hosted' is left out: it is a
          ;; sample with dependencies of its own.
          (replace 'check
            (lambda* (#:key tests? unpack-path #:allow-other-keys)
              (when tests?
                (setenv "HOME" "/tmp")
                (with-directory-excursion (string-append "src/" unpack-path)
                  (invoke "go" "test"
                          ;; These run /bin/sh, absent from the build
                          ;; environment.
                          "-skip"
                          (string-join
                           '("TestDirectCommandProvisionerAndConfigConversion"
                             "TestSandboxShellEnvBuffersAndSubprocessKill")
                           "|")
                          "./cmd/..." "./internal/..." "./tests/..."))))))))
    (native-inputs
     (list go-charm-land-bubbles-v2
           go-charm-land-bubbletea-v2
           go-charm-land-lipgloss-v2
           go-github-com-alpkeskin-gotoon
           go-github-com-gorilla-websocket
           go-github-com-itchyny-gojq
           go-github-com-spf13-cobra
           go-github-com-spf13-pflag
           go-github-com-spyzhov-ajson
           go-github-com-zalando-go-keyring
           go-golang-org-x-term
           go-gopkg-in-yaml-v3))
    (home-page "https://github.com/google-gemini/gemini-api-cli")
    (synopsis "Command-line client for the Gemini API")
    (description
     "@command{gemini-api} is a stateless command-line client for the Gemini
API, built for headless use by coding agents, shell scripts and CI jobs.  It
gives access to the text and reasoning models, multimodal file analysis,
image, audio, music and video generation, and managed agents that run in the
background.  Commands describe themselves with @option{--schema} and
@option{--usage}, @option{--dry-run} prints the resolved HTTP request without
sending it, and the output can be streamed text, JSON, TOON or a
@command{jq}-filtered selection.")
    (license license:asl2.0)))
