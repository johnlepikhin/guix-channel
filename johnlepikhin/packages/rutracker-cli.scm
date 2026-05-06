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

(define-module (johnlepikhin packages rutracker-cli)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xdisorg)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (johnlepikhin packages rust-binary)
  #:use-module (johnlepikhin packages rust-crates))

(define-public rutracker-cli
  (package
    (name "rutracker-cli")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri name version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0l04apf2h8c2y73b6bbpcllqvp6697c5xgaiifn7a4x4d0vwk0sb"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:rust rust-binary-1.88
      #:install-source? #f
      #:tests? #f ;tests need network access to rutracker.org
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'create-cc-symlink
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((gcc (assoc-ref inputs "gcc-toolchain"))
                     (bin-dir (string-append (getcwd) "/.cc-bin")))
                (mkdir-p bin-dir)
                (symlink (string-append gcc "/bin/gcc")
                         (string-append bin-dir "/cc"))
                (setenv "PATH" (string-append bin-dir ":" (getenv "PATH")))
                (setenv "CC" (string-append gcc "/bin/gcc"))
                (setenv "HOST_CC" (string-append gcc "/bin/gcc")))))
          (add-after 'install 'install-completions
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin/rutracker-cli"))
                     (bash-comp (string-append
                                 out "/share/bash-completion/completions"))
                     (zsh-comp (string-append
                                out "/share/zsh/site-functions"))
                     (fish-comp (string-append
                                 out "/share/fish/vendor_completions.d")))
                (mkdir-p bash-comp)
                (mkdir-p zsh-comp)
                (mkdir-p fish-comp)
                (invoke "sh" "-c"
                        (string-append bin " completions bash > "
                                       bash-comp "/rutracker-cli"))
                (invoke "sh" "-c"
                        (string-append bin " completions zsh > "
                                       zsh-comp "/_rutracker-cli"))
                (invoke "sh" "-c"
                        (string-append bin " completions fish > "
                                       fish-comp "/rutracker-cli.fish")))))
          (add-after 'install-completions 'wrap-binary
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (wrap-program (string-append out "/bin/rutracker-cli")
                  `("PATH" prefix
                    (,(string-append (assoc-ref inputs "xdg-utils") "/bin")
                     ,(string-append
                       (assoc-ref inputs "wl-clipboard") "/bin")
                     ,(string-append (assoc-ref inputs "xclip") "/bin")
                     ,(string-append (assoc-ref inputs "xsel") "/bin"))))))))))
    (native-inputs (list gcc-toolchain pkg-config))
    (inputs (append (list bash-minimal xdg-utils wl-clipboard xclip xsel)
                    (cargo-inputs 'rutracker-cli
                                  #:module '(johnlepikhin packages rust-crates))))
    (home-page "https://github.com/johnlepikhin/rutracker-cli")
    (synopsis "Command-line, TUI and MCP client for rutracker.org")
    (description
     "@command{rutracker-cli} is a Rust client for rutracker.org with three
interfaces: a traditional command-line tool, an interactive terminal UI
built with @code{ratatui}, and a Model Context Protocol server for use by
AI assistants.  It supports searching, downloading torrent files, copying
magnet links, and triggering an external torrent client (e.g.@: a remote
Transmission daemon via RPC).  Configuration follows XDG conventions at
@file{$XDG_CONFIG_HOME/rutracker-cli/config.yaml}.")
    (license (list license:expat license:asl2.0))))
