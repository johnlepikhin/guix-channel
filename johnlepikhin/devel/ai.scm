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

(define-module (johnlepikhin devel ai)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (gnu home services guix)
  #:use-module (gnu packages)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages node)
  #:use-module (johnlepikhin packages ai)
  #:use-module (johnlepikhin packages ast-index)
  #:use-module (johnlepikhin packages opencode-ai)
  #:use-module (srfi srfi-1)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:export (home-devel-ai-configuration
            home-devel-ai-service-type))

(define-record-type* <home-devel-ai-configuration>
  home-devel-ai-configuration make-home-devel-ai-configuration
  home-devel-ai-configuration?
  ;; Enable MCP integration for claude-code
  (enable-mcp-integration? home-devel-ai-configuration-enable-mcp-integration?
                           (default #t))
  ;; Additional packages to include
  (extra-packages home-devel-ai-configuration-extra-packages
                  (default '())))

(define (add-packages config)
  (append
   (list
    ;; Core AI development tools
    ast-index
    claude-code
    opencode)
   (home-devel-ai-configuration-extra-packages config)))

(define (add-mcp-configuration config)
  "Generate MCP configuration file for claude-code."
  (if (home-devel-ai-configuration-enable-mcp-integration? config)
      `((".mcp.json"
         ,(plain-file "mcp.json"
                      (string-append
                       "{\n"
                       "  \"mcpServers\": {\n"
                       "  }\n"
                       "}\n"))))
      '()))

(define (add-environment-variables config)
  "Add environment variables for AI development tools."
  `(("CLAUDE_MCP_ENABLED" . "1")
    ;; Optional: Add additional environment variables
    ;; ("CLAUDE_PROJECT_DIR" . "$HOME/projects")
    ))

(define (add-activation-script config)
  "Generate activation script for AI development environment."
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))
        ;; Ensure MCP directory exists
        (mkdir-p (string-append (getenv "HOME") "/.config/claude-code"))
        ;; Create a helper script to start claude-code
        (let ((script-path (string-append (getenv "HOME") "/.local/bin/ai-dev")))
          (mkdir-p (dirname script-path))
          (call-with-output-file script-path
            (lambda (port)
              (format port "#!/bin/sh\n")
              (format port "# AI Development Environment Helper\n")
              (format port "# Usage: ai-dev [project-directory]\n\n")
              (format port "PROJECT_DIR=\"${1:-.}\"\n")
              (format port "cd \"$PROJECT_DIR\"\n\n")
              (format port "# Start claude-code\n")
              (format port "echo \"Starting Claude Code...\"\n")
              (format port "claude\n")))
          (chmod script-path #o755)))))

(define home-devel-ai-service-type
  (service-type
   (name 'home-devel-ai)
   (extensions
    (list
     ;; Add packages
     (service-extension home-profile-service-type
                        add-packages)
     ;; Add MCP configuration
     (service-extension home-files-service-type
                        add-mcp-configuration)
     ;; Add environment variables
     (service-extension home-environment-variables-service-type
                        add-environment-variables)
     ;; Add activation script
     (service-extension home-activation-service-type
                        add-activation-script)))
   (default-value (home-devel-ai-configuration))
   (description "Install and configure AI development tools including
claude-code with MCP integration support.")))
