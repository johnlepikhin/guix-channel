;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Evgenii Lepikhin <johnlepikhin@gmail.com>
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

(define-module (johnlepikhin packages xplugd)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public xplugd
  (package
    (name "xplugd")
    (version "1.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/johnlepikhin/xplugd")
             (commit "64450a31d48129c2352cec3069d26a54aff4dadb")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rimjxq3mfch2d4znsgp76b3y58naq6k7xinzk0r729ysjyzffl6"))))
    (build-system gnu-build-system)
    (native-inputs (list autoconf automake pkg-config))
    (inputs (list libx11 libxi libxrandr))
    (home-page "https://github.com/johnlepikhin/xplugd")
    (synopsis "Monitor hotplug events for X11 displays and input devices")
    (description
     "xplugd is a daemon that monitors hotplug events in X11 for displays,
keyboards, and mice.  It automatically runs a user script when devices are
connected or disconnected, making it ideal for laptop docking stations and
dynamic display configurations with lightweight window managers like Awesome WM,
i3, or Fluxbox.")
    (license license:expat)))
