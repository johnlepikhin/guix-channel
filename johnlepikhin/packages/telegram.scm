;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Raghav Gururajan <rg@raghavgururajan.name>
;;; Copyright © 2022 Hilton Chain <hako@ultrarare.space>
;;; Copyright © 2023-2024 Evgenii Lepikhin <johnlepikhin@gmail.com>
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

(define-module (johnlepikhin packages telegram)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages animation)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages c)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages digest)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages fcitx)
  #:use-module (gnu packages fcitx5)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages hunspell)
  #:use-module (gnu packages image)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages language)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages lxqt)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages telephony)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt))

(define %telegram-version "4.15.0")

(define libyuv-for-telegram-desktop
  (let ((commit "77c2121f7e6b8e694d6e908bbbe9be24214097da")
        (revision "2211"))
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://chromium.googlesource.com/libyuv/libyuv")
            (commit commit)))
      (file-name (git-file-name
                  "libyuv-for-telegram-desktop"
                  (git-version "0" revision commit)))
      (sha256
       (base32
        "1b4k8yskr9ffl5k8s9i0af1gn1pavsfixla26vh8bij69rdr7f9c")))))

(define cmake-helpers-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/cmake_helpers.git")
          (commit "a46279fcfe69ebcc806bb31679ccece5f7c07508")))
    (file-name
     (git-file-name "cmake-helpers-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "0vxnzc0c3akz0i8qq3a683sbavzs0lbqsx3942nsz4sp8jwp2cgv"))))

(define codegen-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/codegen.git")
          (commit "afed06a4c04d1a1cf7cfce4faca273e1f574462e")))
    (file-name
     (git-file-name "codegen-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "06h8z0yy9vfrwrh287aq6y6rfw3camvah7n38m4mi9m4d378f4b9"))))

(define lib-base-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_base.git")
          (commit "888a19075b569eda3d18a977543320823b984ae0")))
    (file-name
     (git-file-name "lib-base-for-telegram-desktop" %telegram-version))
    (patches
     (parameterize
         ((%patch-path
           (map (lambda (directory)
                  (string-append directory "/johnlepikhin/packages/patches"))
                %load-path)))
       ;; Authored by Saku Laesvuori <saku@laesvuori.fi>
       (search-patches "telegram-desktop-fix-lib-base-build.patch")))
    (sha256
     (base32
      "08d2f7ljdfsal7j40d2ikqvg8zfay6f3f3mvzaqpnnm84f9k7k4b"))))

(define lib-crl-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_crl.git")
          (commit "078006d29af0002e6cd8c61a405cdeaf65b37142")))
    (file-name
     (git-file-name "lib-crl-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "18n88ghj3akpkxvllrh1rs19vd0d3xw87hd67qphr30b6ggqs9fd"))))

(define lib-lottie-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_lottie.git")
          (commit "1a700e5a0d7c3e2f617530354ff2a47c5c72bb4a")))
    (file-name
     (git-file-name "lib-lottie-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "18w35sz6k3wcv07v0szx3avpfdl0rjby6yqm1fzmx7fqw2jn6wpl"))))

(define lib-qr-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_qr.git")
          (commit "501f4c3502fd872ab4d777df8911bdac32de7c48")))
    (file-name
     (git-file-name "lib-qr-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "0hmwqj7a9vcy8wq7pd1qprl68im3zl5f1wzcn2zzk2wvi0389k9f"))))

(define lib-rpl-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_rpl.git")
          (commit "8b1015d1bd57ef03fcd07a3eeddd3f5a9b688ade")))
    (file-name
     (git-file-name "lib-rpl-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "12sdhaqby5vlvd5jsj12b3xsqiaknqvijv9ydlyxclx8zail64lv"))))

(define lib-spellcheck-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_spellcheck.git")
          (commit "96543c1716d3790ef12bdec6b113958427710441")))
    (file-name
     (git-file-name "lib-spellcheck-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "0q4346fj7z76lr4dsf6hxb6dxi2i7rppwp61j77hxi51b25n984l"))))

(define lib-storage-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_storage.git")
          (commit "0971b69ca90f1697ef81276d9820dcd6d26de4ac")))
    (file-name
     (git-file-name "lib-storage-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "0ihbh1ajns0sf42h9992hnawwjr1n439sgb0g4zirn2bj5i1zbdv"))))

(define lib-tl-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_tl.git")
          (commit "36fb95c4de1339d2c8921ad6b2911858c3d0e0fa")))
    (file-name
     (git-file-name "lib-tl-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "03rngnssnqwr7ad05qn64mwgji5fb0r3fp5ybkf951p8phr1jvzk"))))

(define lib-ui-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_ui.git")
          (commit "d4247511355a666903e9a57d821b1eb58884aade")))
    (file-name
     (git-file-name "lib-ui-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "1rc03wp3jknfksli05qkww8448crpqrx5xybjbjsqcsriscd0xc1"))))

(define lib-webrtc-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_webrtc.git")
          (commit "5493af61df5cb90a30b686296521961763a009e0")))
    (file-name
     (git-file-name "lib-webrtc-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "1xax1myljv1v246qpfz7lzir6z18crvc65s17x8a6kdl5d3bapgg"))))

(define lib-webview-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_webview.git")
          (commit "4fce8b1971721da739619acf36da0fe79d614a23")))
    (file-name
     (git-file-name "lib-webview-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "0nz798wl2ax3m1icr5d5img8n0mhsw4l6xa6fsf67wsp6k7mfdpa"))))

(define tgcalls-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/TelegramMessenger/tgcalls.git")
          (commit "b9fa8b84d8abe741183f157218ac038c596a54a5")))
    (file-name
     (git-file-name "tgcalls-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "1swni7xw5l0q376b6rnf9h93arzjqm9rkv7g6hz67742lf9a0x9z"))))

(define lib-tgvoip-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/telegramdesktop/libtgvoip.git")
          (commit "c2e718049cf11bbbe7b6d78b78b2d21f0e0affa0")))
    (file-name
     (git-file-name "lib-tgvoip-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "11yc8i4cdr1p9zl5b2sai09bxvaqfny90fhmw8i77gyl21bmii13"))))

(define cld3-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/google/cld3.git")
          (commit "b48dc46512566f5a2d41118c8c1116c4f96dc661")))
    (file-name
     (git-file-name "cld3-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "0ayrrhfdwrf4260h9fsirkhhfrcvc3qqnh6h9wj3ixij2lq0wwqb"))))

(define libsrtp-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/cisco/libsrtp.git")
          (commit "a566a9cfcd619e8327784aa7cff4a1276dc1e895")))
    (file-name
     (git-file-name "libsrtp-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "1ichw2v9s2mggi5p2wbbmlg55q4r48dxi3ks7ykfcfkmh7pb1w1s"))))

(define cppgir-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://gitlab.com/mnauw/cppgir.git")
          (commit "748a04795616f792f6fe91edf24620f78a2ef65c")))
    (file-name
     (git-file-name "cppgir-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "0g4n322qwnakfbxdk6x7896rhb7n8wpiljvj3gnp2wcq05mknkmc"))))

(define GSL-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/GSL.git")
          (commit "09938e870420b69a01f55c755207c871bc20b4e5")))
    (file-name
     (git-file-name "GSL-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "1df4mmvk0i6jnb3cm1kh9hg3pwjw1216s4rm9i1b6asm9dylicn5"))))

(define QR-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/nayuki/QR-Code-generator.git")
          (commit "720f62bddb7226106071d4728c292cb1df519ceb")))
    (file-name
     (git-file-name "QR-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "0dk9ci5gchxa8gh0hyhlj3d5jwxqlnfm85xyp791ldaia14bkj39"))))

(define libdispatch-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/apple/swift-corelibs-libdispatch.git")
          (commit "ee39300b12a77efd3f2f020e009e42d557adbb29")))
    (file-name
     (git-file-name "libdispatch-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "0nb9pl079q3qvqsxjsgpf4cdzyzizgdysqhvdc6pkxbpdy30iw9p"))))

(define expected-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/TartanLlama/expected.git")
          (commit "292eff8bd8ee230a7df1d6a1c00c4ea0eb2f0362")))
    (file-name
     (git-file-name "expected-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "17akrv80h0n4cfmxwvlvbb8ycqza7y3qqygjyphv95rrabqm9r02"))))

(define fcitx5-qt-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/fcitx/fcitx5-qt.git")
          (commit "413747e761b13bacc5ebd01e20810c64c2f3b6dc")))
    (file-name
     (git-file-name "fcitx5-qt-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "0pirn9f17m48blb7jw589fyxzbihg8dsfsff6gnr98vfvlp88qrd"))))

(define libprisma-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/libprisma.git")
          (commit "adf35ba88160777ce5b8d122630852394c58279f")))
    (file-name
     (git-file-name "libprisma-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "01bv0da6gx1haifjiqrz1bmmvjw8i588pvfszjf6jzizxbnidbyn"))))

(define wayland-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/gitlab-freedesktop-mirrors/wayland.git")
          (commit "b2649cb3ee6bd70828a17e50beb16591e6066288")))
    (file-name
     (git-file-name "wayland-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "1g0j4pvsz2n0c7zhfvwfdypd12c5lhr9rhbkn81jhfjmw4vqymv3"))))

(define wayland-protocols-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/gitlab-freedesktop-mirrors/wayland-protocols.git")
          (commit "4624cfaaf563cd7be5e2e2087c8de6d3a48ea867")))
    (file-name
     (git-file-name "wayland-protocols-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "0kpyvnzlwfj9d57v43z5fhk7fliz6224m4hw1xj425c8vrjbw0nx"))))

(define plasma-wayland-protocols-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/KDE/plasma-wayland-protocols.git")
          (commit "78fc6ee77334a147986f01c6d3c6e1b99af1a333")))
    (file-name
     (git-file-name "plasma-wayland-protocols-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "07gwh8qzvxy4bljfrsalkn35m0y6vc10ab9laaqbibspbcq5h3dk"))))

(define kcoreaddons-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/KDE/kcoreaddons.git")
          (commit "79b99f162b200413671dbabe21c73356d9956e35")))
    (file-name
     (git-file-name "kcoreaddons-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "0iij79fqkv6bqmpqryrdlfqdvf8jqrwyff3476a55flw7dclnxnf"))))

(define-public webrtc-for-telegram-desktop
  (let ((commit "dcb5069ff76bd293e86928804208737e6cee2ccc")
        (revision "327"))
    (hidden-package
     (package
       (name "webrtc-for-telegram-desktop")
       (version
        (git-version "0" revision commit))
       (source
        (origin
          (method git-fetch)
          (uri
           (git-reference
            (url "https://github.com/desktop-app/tg_owt.git")
            (commit commit)))
          (file-name
           (git-file-name name version))
          (sha256
           (base32 "047wjgpl6473wxynb080347nskbybsx0kg88hy4yj3mzmnrpxp48"))
          ;; (patches
          ;;  (search-patches
          ;;   ;; https://github.com/desktop-app/tg_owt/pull/101
          ;;   "webrtc-for-telegram-desktop-fix-gcc12-cstdint.patch"))
          (modules '((guix build utils)
                     (ice-9 ftw)
                     (srfi srfi-1)))
          (snippet
           #~(begin
               (let ((keep
                      '("abseil-cpp" "rnnoise"
                        ;; Not available in Guix.
                        "pffft")))
                 (with-directory-excursion "src/third_party"
                   (for-each delete-file-recursively
                             (lset-difference string=?
                                              (scandir ".")
                                              (cons* "." ".." keep)))))
               ;; Unbundle openh264.
               (substitute* "CMakeLists.txt"
			    (("\\include\\(cmake\\/libopenh264\\.cmake\\)")""))))))
       (build-system cmake-build-system)
       (arguments
        (list
         #:tests? #f                    ; No target
         #:phases
         #~(modify-phases %standard-phases
             (add-after 'unpack 'unpack-additional-sources
               (lambda _
                 (let* ((third-party (string-append (getcwd) "/src/third_party"))
                        (crc32c-to (string-append third-party "/crc32c/src"))
                        (libyuv-to (string-append third-party "/libyuv"))
                        (abseil-cpp-to (string-append third-party "/abseil-cpp"))
                        (libsrtp-to (string-append third-party "/libsrtp")))
                   (copy-recursively #$(package-source crc32c) crc32c-to)
                   (copy-recursively #$(package-source abseil-cpp-cxxstd17) abseil-cpp-to)
                   (copy-recursively #$libsrtp-for-telegram-desktop libsrtp-to)
                   (copy-recursively #$libyuv-for-telegram-desktop
                                     libyuv-to))))
             (add-after 'configure 'configure-libsrtp
               (lambda _
                 (let* ((config-path (string-append (getcwd) "/../source/src/third_party/libsrtp/config.h")))
                   (invoke "sh" "-c" (string-append "cd " (getcwd) "/../source/src/third_party/libsrtp/" " && cmake -DENABLE_OPENSSL=ON ."))
                   (install-file config-path (string-append (getcwd) "/../source/src/third_party/libsrtp_config"))))))))
       (native-inputs (list pkg-config python-wrapper yasm))
       (inputs
        (list abseil-cpp-cxxstd17
              crc32c
              ffmpeg
              glib-2.78
              libdrm
              libglvnd
              libjpeg-turbo
              libpcap
              libvpx
              libxcomposite
              libxdamage
              libxext
              libxfixes
              libxrandr
              libxrender
              libxtst
              mesa
              openh264
              openssl
              opus
              pipewire
              protobuf))
       (synopsis "WebRTC support for Telegram Desktop")
       (description "WebRTC-for-Telegram-Desktop is a custom WebRTC fork by
Telegram project, for its use in telegram desktop client.")
       (home-page "https://github.com/desktop-app/tg_owt")
       (license
        (list
         ;; Abseil-CPP
         license:asl2.0
         ;; LibYuv
         (license:non-copyleft "file:///src/third_party/libyuv/LICENSE")
         ;; PFFFT
         (license:non-copyleft "file:///src/third_party/pffft/LICENSE")
         ;; RnNoise
         license:gpl3
         ;; LibSRTP, Crc32c and Others
         license:bsd-3))))))

(define-public rlottie-for-telegram-desktop
  (let ((commit "8c69fc20cf2e150db304311f1233a4b55a8892d7")
        (revision "678"))
    (hidden-package
     (package
       (inherit rlottie)
       (version
        (git-version "0.0.1" revision commit))
       (source
        (origin
          (method git-fetch)
          (uri
           (git-reference
            (url "https://github.com/desktop-app/rlottie.git")
            (commit commit)))
          (file-name
           (git-file-name "rlottie-for-telegram-desktop" version))
          (sha256
           (base32 "14gwg3sn6xdx9ymnx5r0vfm4pk8dwk92s10a1wdvfbjpyrxll64i"))
          (modules '((guix build utils)))
          (snippet
           #~(begin
               (substitute* "meson.build"
                 (("werror=true") "werror=false"))))))))))

(define-public glib-2.78
  (package
    (name "glib")
    (version "2.78.1")
    (source
     (origin
      (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/"
                       name "/" (string-take version 4) "/"
                       name "-" version ".tar.xz"))
       (sha256
	(base32 "1ckqxmpiwvxbgm57bmy4m9cww3v7zgwf4ciqml76azahz38c6nwi"))))
    (build-system meson-build-system)
    (arguments
     (list #:tests? #f))
    (native-inputs
     (list dbus
           gettext-minimal
           m4                           ;for installing m4 macros
           perl                         ;needed by GIO tests
           pkg-config
           python                       ;for 'patch-python-references
           python-wrapper
           tzdata-for-tests))           ;for tests/gdatetime.c
    (inputs
     (list ;; "python", "python-wrapper" and "bash-minimal"
      ;; are for the 'patch-shebangs' phase, to make
      ;; sure the installed scripts end up with a correct shebang
      ;; when cross-compiling.
      bash-minimal
      python
      python-wrapper))
    (propagated-inputs
     (list libffi             ;in the Requires.private field of gobject-2.0.pc
           pcre2               ;in the Requires.private field of glib-2.0.pc
           `(,util-linux "lib")  ;for libmount
           zlib))                ;in the Requires.private field of glib-2.0.pc

    (synopsis "Low-level core library for GNOME projects")
    (description "GLib provides the core application building blocks for
libraries and applications written in C.  It provides the core object system
used in GNOME, the main loop implementation, and a large set of utility
functions for strings and common data structures.")
    (home-page "https://wiki.gnome.org/Projects/GLib")
    (license license:lgpl2.1+)
    (properties '((hidden? . #t)))))

(define glibmm-2.78
  (package
    (name "glibmm")
    (version "2.78.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/glibmm/"
                                  (version-major+minor version)
                                  "/glibmm-" version ".tar.xz"))
              (sha256
               (base32
                "0qaz111vhk1dq9szapcpz91kiyq6ck9ixl9f26g41hr6bnbz4wzl"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     (list
      #:configure-flags #~(list "-Dbuild-documentation=true")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-failing-tests
            (lambda _
              (substitute* "tests/meson.build"
                ;; This test uses /etc/fstab as an example file to read from;
                ;; disable it.
                (("[ \t]*.*giomm_simple.*$") "")
                ;; This test does a DNS lookup, and then expects to be able to
                ;; open a TLS session; just skip it.
                (("[ \t]*.*giomm_tls_client.*$") ""))))
          (add-after 'install 'move-doc
            (lambda _
              (mkdir-p (string-append #$output:doc "/share"))
              (rename-file
               (string-append #$output "/share/doc")
               (string-append #$output:doc "/share/doc")))))))
    (native-inputs
     (list graphviz
           doxygen
           glib-2.78
           m4
           mm-common
           perl
           pkg-config
           cmake
           libxslt))
    (propagated-inputs
     (list libsigc++ glib-2.78))
    (home-page "https://gtkmm.org/")
    (synopsis "C++ interface to the GLib library")
    (description
     "Glibmm provides a C++ programming interface to the part of GLib that are
useful for C++.")
    (license license:lgpl2.1+)))

(define-public c++-gsl-4
  (package
    (name "c++-gsl")
    (version "4.0.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/microsoft/GSL.git")
         (commit
          (string-append "v" version))))
       (file-name
        (git-file-name name version))
       (sha256
        (base32 "0dgb3rb6x2276d3v7x568m3zhqr67rhk8ynqgm3c304avnmcaw3i"))))
    (build-system cmake-build-system)
    (native-inputs
     (list googletest pkg-config))
    (synopsis "Guidelines Support Library")
    (description "c++-gsl contains functions and types that are suggested for
use by the C++ Core Guidelines maintained by the Standard C++ Foundation.")
    (home-page "https://github.com/microsoft/GSL/")
    (license license:expat)))

(define-public telegram-desktop-next
  (package
    (name "telegram-desktop")
    (version %telegram-version)
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/telegramdesktop/tdesktop.git")
         (commit
          (string-append "v" %telegram-version))))
       (file-name
        (git-file-name name version))
       (sha256
        (base32 "0590qba1q8ra5yvj3fk77fkgwha192iqs5hwb10zf5ri8xn7g7bd"))
       (patches
	(parameterize
            ((%patch-path
              (map (lambda (directory)
                     (string-append directory "/johnlepikhin/packages/patches"))
                   %load-path)))
	  (search-patches
           ;; https://github.com/telegramdesktop/tdesktop/pull/24126
           "telegram-desktop-allow-disable-libtgvoip-rev2.patch")))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-1)))
       (snippet
        #~(begin
            (let ((keep
                   '(;; Not available in Guix.
                     "tgcalls")))
              (with-directory-excursion "Telegram/ThirdParty"
                (for-each delete-file-recursively
                          (lset-difference string=?
                                           (scandir ".")
                                           (cons* "." ".." keep)))))))))
    (build-system qt-build-system)
    (arguments
     (list #:tests? #f                      ; No target
           #:imported-modules
           `(,@%qt-build-system-modules
             (guix build glib-or-gtk-build-system))
           #:modules
           '((guix build qt-build-system)
             ((guix build glib-or-gtk-build-system)
              #:prefix glib-or-gtk:)
             (guix build utils)
             (ice-9 match))
           #:configure-flags
           #~(list
              ;; Client applications must provide their own API-ID and API-HASH,
              ;; see also <https://core.telegram.org/api/obtaining_api_id>.
              ;; Here, we snarf the keys from the official Snaps, which are
              ;; also stored in <#$source/snap/snapcraft.yaml>.
              "-DTDESKTOP_API_ID=611335"
              "-DTDESKTOP_API_HASH=d524b414d21f4d37f08684c1df41ac9c"
              "-DTDESKTOP_DISABLE_LEGACY_TGVOIP=ON"
              "-DDESKTOP_APP_DISABLE_CRASH_REPORTS=ON"
              "-DDESKTOP_APP_DISABLE_AUTOUPDATE=ON"
              "-DDESKTOP_APP_USE_PACKAGED_RLOTTIE=ON"
              "-DDESKTOP_APP_DISABLE_SCUDO=ON"
              ;; Do not generate the debug symbols to reduce link time memory
              ;; requirements from 25 GiB to 1.3 GiB.  This also nearly halves
              ;; the build time.
              "-DCMAKE_BUILD_TYPE=Release")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'unpack-additional-sources
                 (lambda _
                   (for-each make-file-writable (find-files "."))
                   (for-each
                    (match-lambda
                      ((dst src)
                       (copy-recursively src dst)
                       (for-each make-file-writable (find-files dst))))
                    '(("cmake" #$cmake-helpers-for-telegram-desktop)
                      ("cmake/external/glib/cppgir" #$cppgir-for-telegram-desktop)
                      ("cmake/external/glib/cppgir/expected-lite" #$expected-lite)
                      ("Telegram/codegen" #$codegen-for-telegram-desktop)
                      ("Telegram/lib_base" #$lib-base-for-telegram-desktop)
                      ("Telegram/lib_crl" #$lib-crl-for-telegram-desktop)
                      ("Telegram/lib_lottie" #$lib-lottie-for-telegram-desktop)
                      ("Telegram/lib_qr" #$lib-qr-for-telegram-desktop)
                      ("Telegram/lib_rpl" #$lib-rpl-for-telegram-desktop)
                      ("Telegram/lib_spellcheck" #$lib-spellcheck-for-telegram-desktop)
                      ("Telegram/lib_storage" #$lib-storage-for-telegram-desktop)
                      ("Telegram/lib_tl" #$lib-tl-for-telegram-desktop)
                      ("Telegram/lib_ui" #$lib-ui-for-telegram-desktop)
                      ("Telegram/lib_webrtc" #$lib-webrtc-for-telegram-desktop)
                      ("Telegram/lib_webview" #$lib-webview-for-telegram-desktop)
                      ("Telegram/ThirdParty/kcoreaddons" #$kcoreaddons-for-telegram-desktop)
                      ("Telegram/ThirdParty/wayland" #$wayland-for-telegram-desktop)
                      ("Telegram/ThirdParty/wayland-protocols" #$wayland-protocols-for-telegram-desktop)
                      ("Telegram/ThirdParty/plasma-wayland-protocols" #$plasma-wayland-protocols-for-telegram-desktop)
                      ("Telegram/ThirdParty/GSL" #$GSL-for-telegram-desktop)
                      ("Telegram/ThirdParty/QR" #$QR-for-telegram-desktop)
                      ("Telegram/ThirdParty/dispatch" #$libdispatch-for-telegram-desktop)
                      ("Telegram/ThirdParty/expected" #$expected-for-telegram-desktop)
                      ("Telegram/ThirdParty/fcitx5-qt" #$fcitx5-qt-for-telegram-desktop)
                      ("Telegram/ThirdParty/libprisma" #$libprisma-for-telegram-desktop)
                      ("Telegram/ThirdParty/cld3" #$cld3-for-telegram-desktop)
                      ("Telegram/ThirdParty/libtgvoip" #$lib-tgvoip-for-telegram-desktop)
                      ("Telegram/ThirdParty/tgcalls" #$tgcalls-for-telegram-desktop)))))
               (add-after 'unpack-additional-sources 'fix-glibmm
                (lambda _
                  (substitute* "cmake/external/glibmm/CMakeLists.txt"
                    (("glibmm-2.4") "glibmm-2.78")
                    (("giomm-2.4") "giomm-2.78"))))
               (add-after 'install 'glib-or-gtk-compile-schemas
                 (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-compile-schemas))
               (add-after 'glib-or-gtk-compile-schemas 'glib-or-gtk-wrap
                 (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap)))))
    (native-inputs
     (list glib-2.78
           `(,gtk+ "bin")
           pkg-config
           python-wrapper))
    (inputs
     (list abseil-cpp-cxxstd17
           alsa-lib
           boost
           c++-gsl-4
           crc32c
           fcitx-qt5
           fcitx5-qt
           ffmpeg
           fmt
           glib-2.78
           glibmm-2.78
           gobject-introspection
           gtk+
           hime
           hunspell
           kcoreaddons
           lib-tgvoip-for-telegram-desktop
           libjpeg-turbo
           libvpx
           libxcb
           lz4
           minizip
           nimf
           openal
           openssl
           opus
           protobuf
           pulseaudio
           qrcodegen-cpp
           qtbase-5
           qtdeclarative-5
           qtimageformats-5
           qtsvg-5
           qtwayland-5
           range-v3
           rlottie-for-telegram-desktop
           rnnoise
           wayland
           webkitgtk
           webrtc-for-telegram-desktop
           xcb-util-keysyms
           xxhash
           zlib))
    (synopsis "Telegram Desktop")
    (description "Telegram desktop is the official desktop version of the
Telegram instant messenger.")
    (home-page "https://desktop.telegram.org/")
    (license
     (list
      ;; ThirdParty
      license:lgpl3
      ;; Others
      license:gpl3+))))

(define-public tl-parser
  (let ((commit "1933e76f8f4fb74311be723b432e4c56e3a5ec06")
        (revision "21"))
    (package
      (name "tl-parser")
      (version
       (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/vysheng/tl-parser.git")
           (commit commit)))
         (file-name
          (git-file-name name version))
         (sha256
          (base32 "13cwi247kajzpkbl86hnwmn1sn2h6rqndz6khajbqj0mlw9mv4hq"))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f                    ; No target
         #:imported-modules
         ((guix build copy-build-system)
          ,@%cmake-build-system-modules)
         #:modules
         (((guix build copy-build-system)
           #:prefix copy:)
          (guix build cmake-build-system)
          (guix build utils))
         #:phases
         (modify-phases %standard-phases
           (replace 'install
             (lambda args
               (apply (assoc-ref copy:%standard-phases 'install)
                      #:install-plan
                      '(("." "bin"
                         #:include ("tl-parser"))
                        ("../source" "include/tl-parser"
                         #:include-regexp ("\\.h$")))
                      args))))))
      (synopsis "Parse tl scheme to tlo")
      (description "TL-Parser is a tl scheme to tlo file parser.  It was
formerly a part of telegram-cli, but now being maintained separately.")
      (home-page "https://github.com/vysheng/tl-parser")
      (license license:gpl2+))))

(define-public tgl
  (let ((commit "ffb04caca71de0cddf28cd33a4575922900a59ed")
        (revision "181"))
    (package
      (name "tgl")
      (version
       (git-version "2.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/vysheng/tgl.git")
           (commit commit)))
         (file-name
          (git-file-name name version))
         (sha256
          (base32 "0cf5s7ygslb5klg1qv9qdc3hivhspmvh3zkacyyhd2yyikb5p0f9"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                    ; No target
         #:imported-modules
         ((guix build copy-build-system)
          ,@%gnu-build-system-modules)
         #:modules
         (((guix build copy-build-system)
           #:prefix copy:)
          (guix build gnu-build-system)
          (guix build utils))
         #:configure-flags
         (list
          ;; Use gcrypt instead of openssl.
          "--disable-openssl"
          ;; Enable extended queries system.
          "--enable-extf"
          ;; Include libevent-based net and timers.
          "--enable-libevent")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'trigger-bootstrap
             (lambda _
               (delete-file "configure")
               #t))
           (add-after 'trigger-bootstrap 'patch-tl-parser
             (lambda _
               (delete-file "Makefile.tl-parser")
               (substitute* "Makefile.in"
                 (("include \\$\\{srcdir\\}/Makefile\\.tl-parser")
                  "")
                 (("\\$\\{EXE\\}/tl-parser")
                  "tl-parser"))
               #t))
           (replace 'install
             (lambda args
               (apply (assoc-ref copy:%standard-phases 'install)
                      #:install-plan
                      '(("bin" "bin")
                        ("." "include/tgl"
                         #:include-regexp ("\\.h$"))
                        ("libs" "lib/tgl"))
                      args))))))
      (native-inputs
       (list autoconf automake libtool pkg-config))
      (inputs
       (list libevent libgcrypt tl-parser zlib))
      (synopsis "Telegram Library")
      (description "TGL is the telegram library for telegram-cli.")
      (home-page "https://github.com/vysheng/tgl")
      (license license:lgpl2.1+))))

(define-public telegram-cli
  (let ((commit "6547c0b21b977b327b3c5e8142963f4bc246187a")
        (revision "324"))
    (package
      (name "telegram-cli")
      (version
       (git-version "1.3.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/vysheng/tg.git")
           (commit commit)))
         (file-name
          (git-file-name name version))
         (sha256
          (base32 "0c1w7jgska71jjbvg1y09v52549pwa4zkdjly18yxywn7gayd2p6"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                    ; No target
         #:imported-modules
         ((guix build copy-build-system)
          ,@%gnu-build-system-modules)
         #:modules
         (((guix build copy-build-system)
           #:prefix copy:)
          (guix build gnu-build-system)
          (guix build utils))
         #:configure-flags
         (list
          ;; Use gcrypt instead of openssl.
          "--disable-openssl")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'trigger-bootstrap
             (lambda _
               (delete-file "configure")
               #t))
           (add-after 'trigger-bootstrap 'patch-tgl-and-tlparser
             (lambda* (#:key inputs #:allow-other-keys)
               (for-each delete-file
                         (list
                          "Makefile.tgl"
                          "Makefile.tl-parser"))
               (substitute* "Makefile.in"
                 (("include \\$\\{srcdir\\}/Makefile\\.tl-parser")
                  "")
                 (("include \\$\\{srcdir\\}/Makefile\\.tgl")
                  "")
                 (("-I\\$\\{srcdir\\}/tgl")
                  (string-append "-I" (assoc-ref inputs "tgl")
                                 "/include/tgl"))
                 (("AUTO=auto")
                  (string-append "AUTO=" (assoc-ref inputs "tgl")
                                 "/include/tgl/auto"))
                 (("LIB=libs")
                  (string-append "LIB=" (assoc-ref inputs "tgl")
                                 "/lib/tgl")))
               #t))
           (replace 'install
             (lambda args
               (apply (assoc-ref copy:%standard-phases 'install)
                      #:install-plan
                      '(("bin" "bin")
                        ("." "etc/telegram-cli"
                         #:include-regexp ("\\.pub$")
                         #:exclude ("tg-server.pub")))
                      args))))))
      (native-inputs
       (list autoconf automake libtool pkg-config))
      (inputs
       (list jansson
             libconfig
             libevent
             libgcrypt
             lua
             openssl
             perl
             python
             readline
             tgl
             tl-parser
             zlib))
      (synopsis "Telegram Messenger CLI")
      (description "TG is the command-line interface for Telegram Messenger.")
      (home-page "https://github.com/vysheng/tg")
      (license license:gpl2+))))

(define-public tgcli
  (package
    (name "tgcli")
    (version "0.3.1")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/erayerdin/tgcli")
         (commit (string-append "v" version))))
       (file-name
        (git-file-name name version))
       (sha256
        (base32 "082zim7rh4r8qyscqimjh2sz7998vv9j1i2y2wwz2rgrlhkhly5r"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Test requirements referes to specific versions of packages,
         ;; which are too old. So we patch them to refer to any later versions.
         (add-after 'unpack 'patch-test-requirements
           (lambda _
             (substitute* "dev.requirements.txt"
               (("==") ">="))))
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "pytest" "tests")))))))
    (native-inputs
     `(("coveralls" ,python-coveralls)
       ("pytest" ,python-pytest)
       ("pytest-click" ,python-pytest-click)
       ("pytest-cov" ,python-pytest-cov)
       ("mkdocs" ,python-mkdocs)
       ("mkdocs-material" ,python-mkdocs-material)
       ("requests-mock" ,python-requests-mock)))
    (propagated-inputs
     `(("click" ,python-click)
       ("colorful" ,python-colorful)
       ("requests" ,python-requests)
       ("yaspin" ,python-yaspin)))
    (home-page "https://tgcli.readthedocs.io")
    (synopsis "Telegram Terminal Application")
    (description "TgCli is a telegram client to automate repetitive tasks.")
    (license license:asl2.0)))
