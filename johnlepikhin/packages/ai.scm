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

(define-module (johnlepikhin packages ai)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system node)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages node))

(define-public claude-code
  (package
    (name "claude-code")
    (version "1.0.44")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://registry.npmjs.org/"
                          "@anthropic-ai/claude-code/-/"
                          "claude-code-" version ".tgz"))
       (sha256
        (base32 "0wchfqag6niwr6nq324nljd7jzc7bpwi5jy983y4fxphz87aqxii"))))
    (build-system node-build-system)
    (arguments
     `(#:node ,node-lts
       #:tests? #f  ; Package has no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'build)  ; No build step needed
         (add-after 'install 'patch-binary
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (claude-bin (string-append bin "/claude")))
               ;; The npm install creates the binary symlink, just ensure it's executable
               (when (file-exists? claude-bin)
                 (chmod claude-bin #o755))
               ;; Also make sure the actual cli.js is executable
               (chmod (string-append out "/lib/node_modules/@anthropic-ai/claude-code/cli.js") #o755)
               #t)))
         (add-after 'patch-binary 'remove-bundled-binaries
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; Remove pre-built binaries that cause RUNPATH issues
               (delete-file-recursively 
                (string-append out "/lib/node_modules/@anthropic-ai/claude-code/vendor"))
               #t))))))
    (home-page "https://github.com/anthropics/claude-code")
    (synopsis "AI-powered coding assistant for the terminal")
    (description
     "Claude Code is an agentic coding tool that lives in your terminal.
It helps developers by executing routine tasks, explaining complex code,
and handling git workflows.  You can interact with it using natural language
to streamline your development process.")
    (license license:expat)))
