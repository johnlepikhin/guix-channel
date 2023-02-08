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

(define-module (johnlepikhin packages dbeaver)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:export (make-dbeaver
            dbeaver-21.1.5
            dbeaver))

(define (make-dbeaver version checksum)
  (package
    (name "dbeaver")
    (version version)
    (source (origin
              (method url-fetch)
              (uri (string-append "https://dbeaver.io/files/21.1.5/dbeaver-ce-" version "-linux.gtk.x86_64-nojdk.tar.gz"))
              (sha256
               (base32 checksum))))
   (build-system copy-build-system)
   (arguments
    `(#:install-plan
      `(("configuration/" "dist/configuration/")
        ("dbeaver" "dist/")
        ("dbeaver-ce.desktop" "/share/applications/")
        ("dbeaver.ini" "dist/")
        ("dbeaver.png" "dist/")
        ("features/" "dist/features/")
        ("icon.xpm" "dist/")
        ("licenses/" "dist/licenses/")
        ("META-INF/" "dist/META-INF/")
        ("p2/" "dist/p2/")
        ("plugins/" "dist/plugins/")
        ("readme.txt" "dist/"))
      #:phases
      (modify-phases
       %standard-phases
       (add-after
        'strip 'fix-binary
        (lambda*
            (#:key outputs inputs #:allow-other-keys)
          (let* ((out (assoc-ref outputs "out"))
                 (patchelf (string-append (assoc-ref inputs "patchelf") "/bin/patchelf"))
                 (binary (string-append out "/dist/dbeaver"))
                 (dynamic-linker (string-append (assoc-ref inputs "libc") ,(glibc-dynamic-linker))))
            (system (string-append patchelf " --set-interpreter " dynamic-linker " " binary))
            (mkdir-p (string-append out "/bin"))
            (system (string-append "ln -s " out "/dist/dbeaver " out "/bin/dbeaver"))
            #t))))))
   (native-inputs `(("patchelf" ,patchelf)))
   (inputs
    `(("openjdk" ,(specification->package "openjdk@17"))))
   (synopsis "Free multi-platform database tool for developers, SQL programmers, database administrators and analysts.")
   (description "Supports any database which has JDBC driver (which basically means - ANY database).

After first start, installed into $HOME library must be patched manually with command like this:

dir1=`ls /gnu/store/*/lib/libcairo.so.2|grep -v profile|head -1`; \
dir2=`ls /gnu/store/*/lib/libgtk-3.so.0|grep -v profile|head -1`; \
dir3=`ls /gnu/store/*/lib/libgthread-2.0.so.0|grep -v profile|head -1`; \
rpath=`dirname \"$dir1\"`:`dirname \"$dir2\"`:`dirname \"$dir3\"`; \
patchelf --set-rpath \"$rpath\" ~/.swt/lib/linux/x86_64/libswt-*.so
")
   (home-page "https://dbeaver.io/")
   (license license:asl2.0)))

(define-public dbeaver-21.1.5
  (make-dbeaver "21.1.5" "1zm6yfd1a1kcrzzjygadc6sg1c9vb4ahcbamq6hx0ixjpwfj0yp4"))

(define-public dbeaver dbeaver-21.1.5)
