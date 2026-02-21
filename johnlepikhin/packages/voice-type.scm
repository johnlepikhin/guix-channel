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

(define-module (johnlepikhin packages voice-type)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages rust-crates)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system cargo)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (johnlepikhin packages rust-binary)
  #:use-module (johnlepikhin packages rust-crates))

(define-public voice-type
  (package
    (name "voice-type")
    (version "0.2.0")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/johnlepikhin/voice-type.git")
                   (commit (string-append "v" version))))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "1cqymnmj87y46gfidk3z1asxj4mxnqp855hhmvl9sg09hmbpabfk"))))
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
          (add-after 'install 'wrap-binary
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (wrap-program
                    (string-append out "/bin/voice-type")
                  `("PATH" prefix
                    (,(string-append (assoc-ref inputs "xclip") "/bin")
                     ,(string-append (assoc-ref inputs "xdotool") "/bin"))))))))))
    (native-inputs (list gcc-toolchain pkg-config))
    (inputs (append (list bash-minimal gtk alsa-lib xclip xdotool)
                    (cargo-inputs 'voice-type
                                  #:module '(johnlepikhin packages rust-crates))))
    (home-page "https://github.com/johnlepikhin/voice-type")
    (synopsis "Voice input for Linux with GTK4 and OpenAI Whisper API")
    (description
     "Voice-type is a desktop application for voice input on Linux.  It uses
GTK4 for the user interface, captures audio via ALSA, sends recordings to
the OpenAI Whisper API for speech-to-text transcription, and types the
recognized text into the active window using xdotool and xclip.")
    (license license:expat)))
