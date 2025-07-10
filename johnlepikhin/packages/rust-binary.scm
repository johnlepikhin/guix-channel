;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Evgenii Lepikhin <johnlepikhin@gmail.com>

(define-module (johnlepikhin packages rust-binary)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages elf)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:))

;; Separate rust-src package that can be used with rust-binary
(define-public rust-src-1.88
  (package
    (name "rust-src")
    (version "1.88.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://static.rust-lang.org/dist/"
                          "rust-src-" version ".tar.xz"))
       (sha256
        (base32 "1ivnrw9cqiivldr2hyrdp9wnn96sskynz16q2pjvslsybcp4dryn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (dest (string-append out "/lib/rustlib/src/rust")))
               (invoke "./install.sh"
                       (string-append "--prefix=" out)
                       "--disable-ldconfig")
               ;; The install.sh script places files in a different structure,
               ;; so we need to move them to match the expected layout
               (when (file-exists? (string-append out "/lib/rustlib/src"))
                 (rename-file (string-append out "/lib/rustlib/src")
                             (string-append out "/lib/rustlib/src-temp"))
                 (mkdir-p dest)
                 (rename-file (string-append out "/lib/rustlib/src-temp/rust/library")
                             (string-append dest "/library"))
                 (rename-file (string-append out "/lib/rustlib/src-temp/rust/src")
                             (string-append dest "/src"))
                 (delete-file-recursively (string-append out "/lib/rustlib/src-temp")))))))))
    (home-page "https://www.rust-lang.org")
    (synopsis "Source code for the Rust standard library")
    (description "This package provides the source code for the Rust standard
library, which is used by rust-analyzer and other tools for code completion
and analysis.")
    (license (list license:expat license:asl2.0))))

(define-public rust-binary-1.88
  (package
    (name "rust-binary")
    (version "1.88.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://static.rust-lang.org/dist/"
                          "rust-" version "-x86_64-unknown-linux-gnu.tar.xz"))
       (sha256
        (base32 "1jhvw9kp96z9bs6zg7y0rzf8h8n34an8x8akwam4y5was70kfm3v"))))
    (build-system gnu-build-system)
    (outputs '("out" "cargo" "tools"))
    (arguments
     `(#:validate-runpath? #f  ; Disable runpath validation
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; Install everything to the main output first
               (invoke "./install.sh"
                       (string-append "--prefix=" out)
                       "--disable-ldconfig"))))
         (add-after 'install 'patch-all-binaries
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (patchelf (string-append (assoc-ref inputs "patchelf") "/bin/patchelf"))
                    (gcc-lib (assoc-ref inputs "gcc:lib"))
                    (glibc (assoc-ref inputs "glibc"))
                    (zlib (assoc-ref inputs "zlib"))
                    (ld-so (string-append glibc ,(glibc-dynamic-linker)))
                    (rpath (string-append gcc-lib "/lib:" glibc "/lib:" 
                                         zlib "/lib:" out "/lib")))
               ;; Patch all executables
               (for-each
                (lambda (file)
                  (when (and (file-exists? file)
                            (not (file-is-directory? file))
                            (elf-file? file))
                    (system* patchelf "--set-interpreter" ld-so file)
                    (system* patchelf "--set-rpath" rpath file)))
                (find-files out (lambda (file stat)
                                 (and (eq? (stat:type stat) 'regular)
                                      (not (zero? (logand (stat:mode stat) #o111)))))))
               ;; Also patch shared libraries
               (for-each
                (lambda (lib)
                  (system* patchelf "--set-rpath" rpath lib))
                (find-files out "\\.so")))))
         (add-after 'patch-all-binaries 'split-outputs
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (cargo (assoc-ref outputs "cargo"))
                   (tools (assoc-ref outputs "tools")))
               ;; Move cargo to its own output
               (mkdir-p (string-append cargo "/bin"))
               (rename-file (string-append out "/bin/cargo")
                           (string-append cargo "/bin/cargo"))
               ;; Move tools to their own output
               (mkdir-p (string-append tools "/bin"))
               (for-each
                (lambda (tool)
                  (when (file-exists? (string-append out "/bin/" tool))
                    (rename-file (string-append out "/bin/" tool)
                                (string-append tools "/bin/" tool))))
                '("rustfmt" "cargo-fmt" "rust-analyzer" "clippy-driver" 
                  "cargo-clippy" "rust-gdb" "rust-gdbgui" "rust-lldb"))
               ;; Move corresponding lib files if they exist
               (when (file-exists? (string-append out "/lib/rustlib"))
                 (mkdir-p (string-append cargo "/lib"))
                 (mkdir-p (string-append tools "/lib"))))))
         (add-after 'split-outputs 'wrap-rust-analyzer
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((tools (assoc-ref outputs "tools"))
                    (rust-src (assoc-ref inputs "rust-src"))
                    (ra-bin (string-append tools "/bin/rust-analyzer")))
               (when (and rust-src (file-exists? ra-bin))
                 ;; Create wrapper for rust-analyzer to set RUST_SRC_PATH
                 (rename-file ra-bin (string-append tools "/bin/.rust-analyzer-real"))
                 (call-with-output-file ra-bin
                   (lambda (port)
                     (format port "#!~a
if test -z \"${RUST_SRC_PATH}\";then export RUST_SRC_PATH=~S;fi;
exec -a \"$0\" \"~a\" \"$@\""
                             (which "bash")
                             (string-append rust-src "/lib/rustlib/src/rust/library")
                             (string-append tools "/bin/.rust-analyzer-real"))))
                 (chmod ra-bin #o755))))))))
    (native-inputs
     `(("patchelf" ,patchelf)))
    (inputs
     `(("gcc:lib" ,gcc "lib")
       ("glibc" ,glibc)
       ("zlib" ,zlib)
       ;; Optional: include rust-src for rust-analyzer
       ("rust-src" ,rust-src-1.88)))
    (home-page "https://www.rust-lang.org")
    (synopsis "The Rust programming language - binary distribution")
    (description "This package provides a binary distribution of the Rust
programming language, including the Rust compiler, Cargo package manager,
and various developer tools like rustfmt, clippy, and rust-analyzer.")
    (license (list license:expat license:asl2.0))))

;; Convenience package that includes rust-binary with rust-src
(define-public rust-binary-complete-1.88
  (package
    (inherit rust-binary-1.88)
    (name "rust-binary-complete")
    (outputs '("out"))  ; Single output for meta-package
    (source #f)
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'unpack)
         (delete 'configure) 
         (delete 'build)
         (delete 'check)
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; This is a meta-package, create minimal output
             (let ((out (assoc-ref outputs "out")))
               (mkdir-p out)
               #t))))))
    (propagated-inputs
     `(("rust-binary" ,rust-binary-1.88 "out")
       ("rust-binary-cargo" ,rust-binary-1.88 "cargo")
       ("rust-binary-tools" ,rust-binary-1.88 "tools")
       ("rust-src" ,rust-src-1.88)))
    (native-inputs '())
    (inputs '())
    (synopsis "Complete Rust binary distribution with source code")
    (description "This meta-package provides a complete Rust development
environment including the compiler, Cargo, development tools (rustfmt, clippy,
rust-analyzer), and the Rust source code for IDE integration.")))
