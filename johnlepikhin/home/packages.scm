(define-module (johnlepikhin home packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages image)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages web)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages astronomy)
  #:use-module (gnu packages lsof)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages code)
  #:use-module (gnu packages coq)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages inkscape)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (johnlepikhin packages apcalc)
  #:use-module (johnlepikhin packages dbeaver)
  #:use-module (johnlepikhin packages git-sync)
  #:use-module (johnlepikhin packages one-password)
  #:use-module (johnlepikhin packages oping)
  #:use-module (johnlepikhin packages telegram)
  #:use-module (johnlepikhin packages puppet-lint)
  #:use-module (johnlepikhin packages rust-nightly)
  #:use-module (johnlepikhin packages zoom)
  #:use-module (johnlepikhin packages perl)
  #:use-module (johnlepikhin home bash)
  #:use-module (johnlepikhin home ssh)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages image-viewers)
  #:use-module (gnu packages telegram)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages chromium)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages haskell-apps)
  #:use-module (gnu packages video)
  #:use-module (gnu packages stalonetray)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages dunst)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages gimp)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages base)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages pv)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages node)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages java)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages check)
  #:use-module (gnu packages file)
  #:use-module (gnu packages ocr)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages music)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages certs)
  #:export (%devel-common-tools-packages
            %devel-common-libs-packages
            %xmonad-packages
            %xdesktop-packages
            %tools-packages
            %perl-packages
            %python-packages
            %devel-other-packages
            %rust-packages
            %golang-packages
            %editors-packages
            %mail-packages
            %astro-packages))

(define %devel-common-tools-packages
  (list
   git
   git-lfs
   (list git "send-email")
   (list git "credential-netrc")
   strace
   jq
   gdb
   gcc-toolchain
   lsof
   valgrind
   patchelf
   postgresql
   global
   dbeaver
   puppet-lint
   lxc
   tcpdump
   perl-perlcritic
   socat
   nmap
   tesseract-ocr
   leptonica
   coq
   cmake))

(define %devel-common-libs-packages
  (list
   pkg-config
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
    xmobar
    rxvt-unicode
    xrdb
    copyq
    network-manager-applet
    xhost
    feh
    telegram-desktop-precompiled
    emacs-telega
    ungoogled-chromium
    pasystray
    mplayer
    xset
    xprop
    xdotool
    xwininfo
    xautolock
    setxkbmap
    stalonetray
    mesa-utils
    dunst
    xsetroot
    xev
    xclip
    xkbcomp
    pavucontrol
    xrandr
    brightnessctl
    libreoffice
    xdg-utils
    xdg-desktop-portal-gtk
    xdg-desktop-portal
    qtbase
    mesa
    gimp
    evince
    recordmydesktop
    vlc
    password-store
    browserpass-native
    xmodmap
    virt-manager
    inkscape
    xf86-input-synaptics)))

(define %tools-packages
  (list
   which
   rsync
   rpm
   sshfs
   curl
   cpio
   pwgen
   xz
   netcat
   blueman
   ispell
   aspell-dict-ru
   texlive
   pinentry
   ripgrep
   pigz
   pv
   whois
   (list isc-bind "utils")
   file
   unzip
   1password-cli
   graphviz
   tgcli
   apcalc
   playerctl
   jmtpfs
   macchanger
   wireshark
   zip
   grep
   sed
   psmisc
   procps
   powertop
   youtube-dl
   (make-git-sync "2020-11-09" "aa420e3f9681ce54cb3e2de10bd118f2664621ea" "0wrwmh852a2xjpzsd45fmpg9v1k20fwy5dl7cs5lc5c6k4mhigbi")))

(define %perl-packages
  (list
   perl
   perltidy
   perl-json
   perl-yaml-tiny))

(define %python-packages
  (list
   python
   python-pytest
   python-language-server))

(define %golang-packages
  (list
   go))

(define %devel-other-packages
  (list
   node
   guile-3.0
   emacs-guix
   emacs-geiser
   shellcheck))

(define %rust-packages
  (list
   rust-nightly-2022.03.13
   clippy-nightly-2022.03.13
   rust-src-nightly-2022.03.13
   clang
   openssl))

(define %editors-packages
  (list
   emacs
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
