
(define-module (johnlepikhin packages rust-nightly)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages base)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages compression)
  #:use-module (guix build-system copy)
  #:export (make-rust-nightly
            make-rust-src-nightly))

(define (make-rust-nightly timestamp checksum)
  (package
   (name "rust")
   (version (string-append "nightly-" timestamp))
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://static.rust-lang.org/dist/"
                  timestamp
                  "/rust-nightly-x86_64-unknown-linux-gnu.tar.xz"))
            (sha256 (base32 checksum))))
   (build-system copy-build-system)
   (supported-systems '("x86_64-linux"))
   (arguments
    `(
      #:phases
      (modify-phases
       %standard-phases
       (add-after
        'strip 'fix-binary
        (lambda*
            (#:key outputs inputs #:allow-other-keys)
          (let* ((out (assoc-ref outputs "out"))
                 (patchelf (string-append (assoc-ref inputs "patchelf") "/bin/patchelf"))
                 (bin-masks
                  (string-append
                   out "/bin/rustc"
                   " " out "/bin/rustdoc"
                   " " out "/bin/rustfmt"
                   " " out "/bin/cargo"
                   " " out "/bin/cargo-fmt"
                   " " out "/bin/rust-analyzer"
                   " " out "/lib/rustlib/x86_64-unknown-linux-gnu/bin/rust*"
                   " " out "/lib/rustlib/x86_64-unknown-linux-gnu/bin/gcc-ld/*"))
                 (lib-masks (string-append
                             out "/lib/*.so "
                             out "/lib/rustlib/x86_64-unknown-linux-gnu/lib/*.so"))
                 (dynamic-linker (string-append (assoc-ref inputs "libc") ,(glibc-dynamic-linker))))
            (system (string-append
                     patchelf
                     " --set-rpath \"$LIBRARY_PATH:"
                     out
                     "/lib\" --set-interpreter "
                     dynamic-linker " " bin-masks))
            (system (string-append patchelf " --set-rpath \"$LIBRARY_PATH:" out "/lib\" " lib-masks))
            #t))))
      #:install-plan
      `(("rustc/bin/rustc" "bin/")
        ("rustc/bin/rustdoc" "bin/")
        ("rustc/bin/rust-gdb" "bin/")
        ("rustc/bin/rust-gdbgui" "bin/")
        ("rustc/bin/rust-lldb" "bin/")
        ("rustfmt-preview/bin/cargo-fmt" "bin/")
        ("rustfmt-preview/bin/rustfmt" "bin/")
        ("rust-analyzer-preview/bin/rust-analyzer" "bin/")
        ("cargo/bin/cargo" "bin/")
        ("rustc/lib" "lib")
        ("rust-std-x86_64-unknown-linux-gnu/lib/rustlib/x86_64-unknown-linux-gnu/lib"
         "lib/rustlib/x86_64-unknown-linux-gnu/lib"))))
   (native-inputs `(("patchelf" ,patchelf)
                    ("cmake" ,cmake)))
   (inputs
    `(("libstdc++" ,(make-libstdc++ gcc))
      ("gcc:lib" ,(canonical-package gcc) "lib")
      ("glibc" ,glibc)
      ("zlib" ,zlib)))
   (synopsis "Compiler for the Rust programming language")
   (description "Rust is a systems programming language that provides memory safety and thread safety guarantees.")
   (home-page "https://www.rust-lang.org")
   (license (list license:asl2.0 license:expat))))

(define (make-rust-src-nightly timestamp checksum)
  (package
   (name "rust-src")
   (version (string-append "nightly-" timestamp))
   (source (origin
            (method url-fetch)
            (uri (string-append "https://static.rust-lang.org/dist/" timestamp "/rust-src-nightly.tar.xz"))
            (sha256 (base32 checksum))))
   (build-system copy-build-system)
   (supported-systems '("x86_64-linux"))
   (arguments
    `(
      #:install-plan
      `(("rust-src/lib/rustlib" "lib/"))))
   (synopsis "Compiler for the Rust programming language - stdlib sources")
   (description "Rust is a systems programming language that provides memory safety and thread safety guarantees.")
   (home-page "https://www.rust-lang.org")
   (license (list license:asl2.0 license:expat))))

(define (make-clippy-nightly timestamp checksum rust-package)
  (package
    (name "rust-clippy")
    (version (string-append "nightly-" timestamp))
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://static.rust-lang.org/dist/"
                    timestamp
                    "/clippy-nightly-x86_64-unknown-linux-gnu.tar.xz"))
              (sha256 (base32 checksum))))
    (build-system copy-build-system)
    (supported-systems '("x86_64-linux"))
    (arguments
     `(
       #:phases
       (modify-phases
           %standard-phases
         (add-after
             'strip 'fix-binary
           (lambda*
               (#:key outputs inputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (patchelf (string-append (assoc-ref inputs "patchelf") "/bin/patchelf"))
                    (binaries
                     (string-append
                      out "/bin/cargo-clippy "
                      out "/bin/clippy-driver"))
                    (dynamic-linker (string-append (assoc-ref inputs "libc") ,(glibc-dynamic-linker))))
               (system (string-append
                        patchelf
                        " --set-rpath \"$LIBRARY_PATH\" --set-interpreter "
                        dynamic-linker " " binaries))))))
       #:install-plan
       `(("clippy-preview/bin/cargo-clippy" "bin/")
         ("clippy-preview/bin/clippy-driver" "bin/"))))
    (native-inputs `(("patchelf" ,patchelf)))
    (inputs
     `(("gcc:lib" ,(canonical-package gcc) "lib")
       ("glibc" ,glibc)
       ("rust" ,rust-package)))
    (synopsis "Lints to avoid common pitfalls in Rust")
    (description "This package provides a bunch of helpful lints to avoid common pitfalls in Rust.")
    (home-page " https://github.com/rust-lang/rust-clippy")
    (license (list license:asl2.0 license:expat))))

(define-public rust-nightly-2021.05.21
  (make-rust-nightly "2021-05-21" "1r3b45krsaffbpbnla02y797x1lab5hp4x95mrp2bkq0clyzyk70"))

(define-public rust-src-nightly-2021.05.21
  (make-rust-src-nightly "2021-05-21" "06k4a7j69x2yqkmksihnjkwiy0f3amh1iykscka0iykdfwh2mcsg"))

(define-public clippy-nightly-2021.05.21
  (make-clippy-nightly "2021-05-21" "0wpfrwzmgg0r67kvymx937iffr11bh9nig3xk92dd3kp5dhvrf6j" rust-nightly-2021.05.21))

;;

(define-public rust-nightly-2021.11.11
  (make-rust-nightly "2021-11-11" "1bs2866yc16x9m0lsj4sij67f4zwiq1f3b1b0jqck7m44p71d0dr"))

(define-public rust-src-nightly-2021.11.11
  (make-rust-src-nightly "2021-11-11" "10rjnzzx5pl3xvn3fzz0whpbixg6388qzjfwa9k537bp5dkp8bsf"))

(define-public clippy-nightly-2021.11.11
  (make-clippy-nightly "2021-11-11" "0bcwcra1bs0srwwiyc2hvr1l8rcnjndjcpcwsc6n8584nslg6r99" rust-nightly-2021.11.11))

;;

(define-public rust-nightly-2022.03.13
  (make-rust-nightly "2022-03-13" "1jzz4kf64b8d8lkkxci5a28jwk7wrpz8hnmjqdgpf48h66lslkvh"))

(define-public rust-src-nightly-2022.03.13
  (make-rust-src-nightly "2022-03-13" "0qgpcwcabbrjnc6d9b0i04vg70p8ss0qxs02m5bxp7g764pgb8bf"))

(define-public clippy-nightly-2022.03.13
  (make-clippy-nightly "2022-03-13" "09i3iz689v4m9902xnkp4ia23zphlvwszy589lcpv65clcx0psby" rust-nightly-2022.03.13))
