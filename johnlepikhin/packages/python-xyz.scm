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

(define-module (johnlepikhin packages python-xyz)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages serialization))

(define-public python-overrides
  (package
    (name "python-overrides")
    (version "7.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "overrides" version))
       (sha256
        (base32 "02l2j5pnsxn0q5i63is5g6i3q06ri83nf7lv55swr61vv6iqy5am"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f))  ; Tests have issues with test discovery
    (native-inputs
     (list python-setuptools python-wheel))
    (home-page "https://github.com/mkorpela/overrides")
    (synopsis "Decorator to automatically detect mismatch when overriding a method")
    (description
     "A decorator @override that verifies that a method that should override
an inherited method actually does it.")
    (license license:asl2.0)))

(define-public python-docstring-parser
  (package
    (name "python-docstring-parser")
    (version "0.16")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "docstring_parser" version))
       (sha256
        (base32 "0vma76kjg2isgy4d57xgc56s6dbcaam3rgdn8q0xpqpi1ayym2sk"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f))  ; Package has no tests
    (native-inputs
     (list python-poetry-core))
    (home-page "https://github.com/rr-/docstring_parser")
    (synopsis "Parse Python docstrings")
    (description
     "This library parses Python docstrings and returns the information
in a structured format.")
    (license license:expat)))

(define-public python-sse-starlette
  (package
    (name "python-sse-starlette")
    (version "2.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sse-starlette" version))
       (sha256
        (base32 "0ylcvrbpvip9y56ph7qhjwzwr4lj09vcwf90k4ixv51p00wyz8j3"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f))  ; Tests require additional dependencies
    (native-inputs
     (list python-hatchling))
    (propagated-inputs
     (list python-starlette
           python-anyio))
    (home-page "https://github.com/sysid/sse-starlette")
    (synopsis "Server-Sent Events for Starlette and FastAPI")
    (description
     "SSE-Starlette provides Server-Sent Events (SSE) for Starlette and FastAPI
applications, enabling real-time server-to-client communication.")
    (license license:bsd-3)))

(define-public python-mcp
  (package
    (name "python-mcp")
    (version "1.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mcp" version))
       (sha256
        (base32 "0zkb1fajng9j4r7mxr25mcfziby0vcqivfhhjvwkqih6gn9f1r8y"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f))  ; Tests require additional dependencies
    (native-inputs
     (list python-hatchling))
    (propagated-inputs
     (list python-pydantic
           python-httpx
           python-sse-starlette))
    (home-page "https://github.com/modelcontextprotocol/python-sdk")
    (synopsis "Model Context Protocol SDK")
    (description
     "The Model Context Protocol (MCP) is an open standard that enables
seamless communication between LLM applications and external data sources,
tools, and services.")
    (license license:expat)))

(define-public python-sensai-utils
  (package
    (name "python-sensai-utils")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://files.pythonhosted.org/packages/c6/df/"
                          "49cc942b653427df0bbd3b9a2fb1e85652df3d4c1d9f6dcbfce1b6155716/"
                          "sensai_utils-" version "-py3-none-any.whl"))
       (sha256
        (base32 "01vmaiayy4xi46vdg786j69z6zgxq05ylr7k7jrl63k2a9swavzd"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f))  ; No tests in PyPI release
    (home-page "https://github.com/appliedAI-Initiative/sensAI")
    (synopsis "Utility functions from the sensAI library")
    (description
     "This package contains utility functions that were originally part of
the sensAI machine learning library.")
    (license license:expat)))

(define-public python-pyright
  (package
    (name "python-pyright")
    (version "1.1.396")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyright" version))
       (sha256
        (base32 "1dzbcighlq2g49ldlxmpss3mxrlz7bz0jha7ysir2kzl1qg5cx29"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:tests? #f))  ; Tests require Node.js
    (native-inputs
     (list python-setuptools python-wheel))
    (propagated-inputs
     (list python-nodeenv))
    (home-page "https://github.com/microsoft/pyright")
    (synopsis "Static type checker for Python")
    (description
     "Pyright is a static type checker for Python that is designed for
high performance and accuracy.")
    (license license:expat)))

(define-public python-serena
  (package
    (name "python-serena")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/oraios/serena")
             (commit "2025-05-19")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xin1fm1zmqxn8yk4z8kw92mizf7pysarzrfm3cqgjnwnixvhkcf"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:tests? #f  ; Tests require additional dependencies and setup
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'relax-python-requirement
           (lambda _
             ;; Allow Python 3.10+ instead of strict 3.11
             (substitute* "pyproject.toml"
               ((">=3.11, <3.12") ">=3.10"))
             #t)))))
    (propagated-inputs
     (list python-requests
           python-overrides
           python-dotenv
           python-flask
           python-pydantic
           python-pyyaml
           python-ruamel.yaml
           python-jinja2
           python-pathspec
           python-psutil
           python-docstring-parser
           python-joblib
           python-tqdm))
    (native-inputs
     (list python-hatchling
           python-pytest))
    (home-page "https://github.com/oraios/serena")
    (synopsis "Open-source coding agent toolkit")
    (description
     "Serena is an open-source coding agent toolkit that provides semantic code
retrieval and editing capabilities.  It turns Large Language Models into coding
agents that can work directly on codebases, supporting multiple programming
languages through language servers.")
    (license license:expat)))
