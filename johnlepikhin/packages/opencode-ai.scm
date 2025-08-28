;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Evgenii Lepikhin <johnlepikhin@gmail.com>
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
  #:use-module (gnu packages elf)
  #:use-module (gnu packages compression))

(define-public opencode
  (package
    (name "opencode")
    (version "0.5.28")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/sst/opencode/releases/download/v"
                           version "/opencode-linux-x64.zip"))
       (sha256
        (base32 "04r5n2w4x5qs15na810bb18d1c0yfhvm0n172xwslvrjjk4lral1"))))
    (build-system trivial-build-system)
    (arguments
     (list #:modules '((guix build utils))
           #:builder
           #~(begin
               (use-modules (guix build utils))
               (let* ((source #$source)
                      (unzip #$(file-append unzip "/bin/unzip"))
                      (patchelf #$(file-append patchelf "/bin/patchelf"))
                      (bash #$(file-append bash-minimal "/bin/bash"))
                      (libc #$(file-append glibc "/lib"))
                      (out #$output)
                      (bin (string-append out "/bin"))
                      (tmpdir "opencode-extract"))
                 
                 ;; Создать директории
                 (mkdir-p bin)
                 (mkdir-p tmpdir)
                 
                 ;; Извлечь архив
                 (invoke unzip source "-d" tmpdir)
                 
                 ;; Скопировать бинарник
                 (copy-file (string-append tmpdir "/opencode")
                            (string-append bin "/.opencode-real"))
                 (chmod (string-append bin "/.opencode-real") #o755)
                 
                 ;; Пропатчить только интерпретатор
                 (invoke patchelf "--set-interpreter"
                         (string-append #$(file-append glibc "/lib") "/ld-linux-x86-64.so.2")
                         (string-append bin "/.opencode-real"))
                 
                 ;; Создать wrapper script
                 (call-with-output-file (string-append bin "/opencode")
                   (lambda (port)
                     (format port "#!~a~%~
                                  export PATH=~a:$PATH~%~
                                  export LD_LIBRARY_PATH=~a:$LD_LIBRARY_PATH~%~
                                  exec ~a \"$@\"~%"
                             bash
                             (string-append #$(file-append coreutils "/bin"))
                             (string-append #$(file-append glibc "/lib"))
                             (string-append bin "/.opencode-real"))))
                 (chmod (string-append bin "/opencode") #o755)
                 
                 ;; Очистить временные файлы
                 (delete-file-recursively tmpdir)))))
    (native-inputs
     (list unzip patchelf))
    (inputs
     (list glibc bash-minimal coreutils))
    (home-page "https://github.com/sst/opencode")
    (synopsis "AI coding agent built for the terminal")
    (description "OpenCode is an AI coding agent that helps with software development
directly from the terminal.  It provides intelligent code completion, debugging
assistance, and automated coding tasks using advanced AI models.")
    (license license:expat)))