;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025, 2026 Evgenii Lepikhin <johnlepikhin@gmail.com>
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

(define-module (johnlepikhin packages opencode-ai)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system trivial)
  #:use-module (guix build utils)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages version-control))

(define-public opencode
  (package
    (name "opencode")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/anomalyco/opencode/releases/download/v"
                           version "/opencode-linux-x64.tar.gz"))
       (sha256
        (base32 "1r27d1cml7rf1k898pw5870lmr69vx115q4x0b7vrdqms3prv0bw"))))
    (build-system trivial-build-system)
    (arguments
     (list #:modules '((guix build utils))
           #:builder
           #~(begin
               (use-modules (guix build utils))
               (let* ((source #$source)
                      (tar (string-append #$(file-append tar "/bin/tar")))
                      (patchelf #$(file-append patchelf "/bin/patchelf"))
                      (bash #$(file-append bash-minimal "/bin/bash"))
                      (out #$output)
                      (bin (string-append out "/bin"))
                      (tmpdir "opencode-extract"))

                 ;; Создать директории
                 (mkdir-p bin)
                 (mkdir-p tmpdir)

                 ;; Извлечь архив (tar.gz содержит единственный файл `opencode`).
                 ;; gzip нужен GNU tar для распаковки `-z`.
                 (setenv "PATH"
                         (string-append #$(file-append gzip "/bin")))
                 (invoke tar "-xzf" source "-C" tmpdir)

                 ;; Скопировать бинарник
                 (copy-file (string-append tmpdir "/opencode")
                            (string-append bin "/.opencode-real"))
                 (chmod (string-append bin "/.opencode-real") #o755)

                 ;; Пропатчить только интерпретатор
                 (invoke patchelf "--set-interpreter"
                         (string-append #$(file-append glibc "/lib")
                                        "/ld-linux-x86-64.so.2")
                         (string-append bin "/.opencode-real"))

                 ;; Создать wrapper script.  Ripgrep и git обязательны в
                 ;; runtime: без `rg` в PATH opencode попытается скачать
                 ;; ripgrep с GitHub при первом запуске (см.
                 ;; packages/opencode/src/file/ripgrep.ts), а git вызывается
                 ;; напрямую из src/shell, src/project, src/cli/cmd/session.
                 (call-with-output-file (string-append bin "/opencode")
                   (lambda (port)
                     (format port "#!~a~%~
                                  export PATH=~a:~a:~a:$PATH~%~
                                  export LD_LIBRARY_PATH=~a:$LD_LIBRARY_PATH~%~
                                  exec ~a \"$@\"~%"
                             bash
                             #$(file-append ripgrep "/bin")
                             #$(file-append git-minimal "/bin")
                             #$(file-append coreutils "/bin")
                             #$(file-append glibc "/lib")
                             (string-append bin "/.opencode-real"))))
                 (chmod (string-append bin "/opencode") #o755)

                 ;; Очистить временные файлы
                 (delete-file-recursively tmpdir)))))
    (native-inputs
     (list tar gzip patchelf))
    (inputs
     (list glibc bash-minimal coreutils ripgrep git-minimal))
    (home-page "https://github.com/anomalyco/opencode")
    (synopsis "Open source AI coding agent for the terminal")
    (description "OpenCode is an AI coding agent that runs directly in the
terminal.  It exposes a TUI interface and a set of tools for code search,
editing and shell execution.  The bundled binary relies on @code{ripgrep}
for fast code search and @code{git} for repository operations; both are
wired into the wrapper so the agent works offline without fetching extra
binaries at first start.")
    (license license:expat)))
