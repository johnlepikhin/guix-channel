;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Evgenii Lepikhin <johnlepikhin@gmail.com>
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

(define-module (johnlepikhin home packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages android)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages astronomy)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bittorrent)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages check)
  #:use-module (gnu packages chromium)
  #:use-module (gnu packages code)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages coq)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages diffoscope)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages file)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages gimp)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnome-xyz)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-apps)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages image-viewers)
  #:use-module (gnu packages inkscape)
  #:use-module (gnu packages java)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages lsof)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages markup)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages music)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages node)
  #:use-module (gnu packages ocr)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages pv)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages rdesktop)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages texlive)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages unicode)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (johnlepikhin home bash)
  #:use-module (johnlepikhin home ssh)
  #:use-module (johnlepikhin packages apcalc)
  #:use-module (johnlepikhin packages dbeaver)
  #:use-module (johnlepikhin packages git-sync)
  #:use-module (johnlepikhin packages golang)
  #:use-module (johnlepikhin packages oping)
  #:use-module (johnlepikhin packages puppet-lint)
  #:use-module (johnlepikhin packages rust-nightly)
  #:use-module (johnlepikhin packages shadowplay)
  #:use-module (johnlepikhin packages telegram)
  #:use-module (nongnu packages messaging)
  #:use-module (nongnu packages chrome)
  #:export (%devel-common-tools-packages
            %devel-common-libs-packages
            %xmonad-packages
            %xdesktop-packages
            %tools-packages
            %devel-other-packages
            %golang-packages
            %editors-packages
            %mail-packages
            %astro-packages
            %mapping-packages))

(define %devel-common-tools-packages
  (list
   git
   git-lfs
   (list git "send-email")
   (list git "credential-netrc")
   strace
   jq
   lsof
   patchelf
   postgresql
   global
   dbeaver
   puppet-lint
   tcpdump
   socat
   nmap
   tesseract-ocr
   tesseract-ocr-tessdata-fast
   leptonica
   coq
   adb
   python-yamllint
   docker-compose))

(define %devel-common-libs-packages
  (list
   cloc
   liboping
   alsa-lib
   libxcomposite
   nss-certs))

(define %xmonad-packages
  (list ghc ghc-hostname ghc-xmonad-contrib xmonad xmessage))

(define %xdesktop-packages
  (append
   %xmonad-packages
   (list
    autocutsel
    brightnessctl
    copyq
    dunst
    evince
    feh
    flameshot
    flatpak
    font-google-noto
    geeqie
    gimp
    google-chrome-beta
    inkscape
    kdenlive
    libfreeaptx
    libreoffice
    markdown ;; required by Emacs Obsidian mode
    mediainfo ;; required by kdenlive
    mesa-utils
    mplayer
    network-manager-applet
    pasystray
    pavucontrol
    qbittorrent
    qogir-icon-theme
    quaternion
    recordmydesktop
    simplescreenrecorder
    smplayer
    telegram-desktop-next
    ungoogled-chromium
    unicode-emoji
    virt-manager
    vlc
    xautolock
    xclip
    xdg-desktop-portal
    xdg-desktop-portal-gtk
    xdg-utils
    xdotool
    xev
    xf86-input-synaptics
    xhost
    xmobar
    xmodmap
    xorg-server ;; provides Xephyr
    xprop
    xrandr
    xrdb
    xset
    xsetroot
    xwininfo
    zoom
    )))

(define %tools-packages
  (list
   (list isc-bind "utils")
   (make-git-sync "2020-11-09" "aa420e3f9681ce54cb3e2de10bd118f2664621ea" "0wrwmh852a2xjpzsd45fmpg9v1k20fwy5dl7cs5lc5c6k4mhigbi")
   apcalc
   aspell
   aspell-dict-en
   aspell-dict-ru
   blueman
   cpio
   curl
   diffoscope
   ffmpeg
   file
   gnuplot
   graphviz
   grep
   jmtpfs
   macchanger
   netcat
   pigz
   playerctl
   powertop
   procps
   psmisc
   pv
   pwgen
   ripgrep
   rpm
   rsync
   sed
   sshfs
   texlive
   tgcli
   tlp
   unzip
   which
   whois
   wireshark
   xz
   yt-dlp
   zip
   zstd
   ))

(define %golang-packages
  (list
   go-1.20
   gopls-go1.20))

(define %devel-other-packages
  (list
   node-lts
   guile-3.0
   emacs-guix
   emacs-geiser
   shellcheck
   openjdk
   rpm
   shadowplay))

(define %editors-packages
  (list
   vim
   emacs-geiser
   emacs-geiser-guile
   emacs-ac-geiser))

(define %mail-packages
  (list
   offlineimap))

(define %astro-packages
  (list
   stellarium
   celestia))

(define %mapping-packages
  (list
   josm))
