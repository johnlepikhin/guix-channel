;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016 Eric Le Bihan <eric.le.bihan.dev@free.fr>
;;; Copyright © 2016 Nikita <nikita@n0.is>
;;; Copyright © 2017 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017, 2018 Nikolai Merinov <nikolai.merinov@member.fsf.org>
;;; Copyright © 2017, 2019-2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Danny Milosavljevic <dannym+a@scratchpost.org>
;;; Copyright © 2019 Ivan Petkov <ivanppetkov@gmail.com>
;;; Copyright © 2020, 2021 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2020 Matthew James Kraai <kraai@ftbfs.org>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 (unmatched parenthesis <paren@disroot.org>
;;; Copyright © 2022 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2022 Jim Newsome <jnewsome@torproject.org>
;;; Copyright © 2022 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2023 Fries <fries1234@protonmail.com>
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

(define-module (johnlepikhin packages rust)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages llvm-meta)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module ((guix build utils) #:select (alist-replace))
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26))

;; This is the hash for the empty file, and the reason it's relevant is not
;; the most obvious.
;;
;; The root of the problem is that Cargo keeps track of a file called
;; Cargo.lock, that contains the hash of the tarball source of each dependency.
;;
;; However, tarball sources aren't handled well by Guix because of the need to
;; patch shebangs in any helper scripts. This is why we use Cargo's vendoring
;; capabilities, where instead of the tarball, a directory is provided in its
;; place. (In the case of rustc, the source code already ships with vendored
;; dependencies, but crates built with cargo-build-system undergo vendoring
;; during the build.)
;;
;; To preserve the advantages of checksumming, vendored dependencies contain
;; a file called .cargo-checksum.json, which contains the hash of the tarball,
;; as well as the list of files in it, with the hash of each file.
;;
;; The patch-cargo-checksums phase of cargo-build-system runs after
;; any Guix-specific patches to the vendored dependencies and regenerates the
;; .cargo-checksum.json files, but it's hard to know the tarball checksum that
;; should be written to the file - and taking care of any unhandled edge case
;; would require rebuilding everything that depends on rust. This is why we lie,
;; and say that the tarball has the hash of an empty file. It's not a problem
;; because cargo-build-system removes the Cargo.lock file. We can't do that
;; for rustc because of a quirk of its build system, so we modify the lock file
;; to substitute the hash.
(define %cargo-reference-hash
  "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")

(define* (nix-system->gnu-triplet-for-rust
          #:optional (system (%current-system)))
  (match system
    ("x86_64-linux"   "x86_64-unknown-linux-gnu")
    ("i686-linux"     "i686-unknown-linux-gnu")
    ("armhf-linux"    "armv7-unknown-linux-gnueabihf")
    ("aarch64-linux"  "aarch64-unknown-linux-gnu")
    ("mips64el-linux" "mips64el-unknown-linux-gnuabi64")
    ("riscv64-linux"  "riscv64gc-unknown-linux-gnu")
    (_                (nix-system->gnu-triplet system))))

(define* (rust-uri version #:key (dist "static"))
  (string-append "https://" dist ".rust-lang.org/dist/"
                 "rustc-" version "-src.tar.gz"))

(define* (rust-bootstrapped-package base-rust version checksum)
  "Bootstrap rust VERSION with source checksum CHECKSUM using BASE-RUST."
  (package
    (inherit base-rust)
    (version version)
    (source
     (origin
       (inherit (package-source base-rust))
       (uri (rust-uri version))
       (sha256 (base32 checksum))))
    (native-inputs
     (alist-replace "cargo-bootstrap" (list base-rust "cargo")
                    (alist-replace "rustc-bootstrap" (list base-rust)
                                   (package-native-inputs base-rust))))))

(define (make-ignore-test-list strs)
  "Function to make creating a list to ignore tests a bit easier."
  (map (lambda (str)
    `((,str) (string-append "#[ignore]\n" ,str)))
    strs))

;;; Note: mrustc's only purpose is to be able to bootstap Rust; it's designed
;;; to be used in source form.
(define %mrustc-commit "597593aba86fa2edbea80c6e09f0b1b2a480722d")
(define %mrustc-source
  (let* ((version "0.10")
         (commit %mrustc-commit)
         (revision "2")
         (name "mrustc"))
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/thepowersgang/mrustc")
            (commit commit)))
      (file-name (git-file-name name (git-version version revision commit)))
      (sha256
       (base32
        "09rvm3zgx1d86gippl8qzh13m641ynbw9q0zsc90g0h1khd3z3b6"))
      (modules '((guix build utils)))
      (snippet
       '(begin
          ;; Drastically reduces memory and build time requirements
          ;; by disabling debug by default.
          (substitute* (find-files "." "Makefile")
            (("-g ") "")))))))

(define-public rust-1.80
  (let ((base-rust (rust-bootstrapped-package rust-1.79 "1.80.0"
                    "0wnn44spsy1gnbwxn4gzbnrgkxah9nrgfxj5mqn6n3r37wcnqq3g")))
    (package
      (inherit base-rust)
      (source
       (origin
         (inherit (package-source base-rust))
         (snippet
          '(begin
             (for-each delete-file-recursively
                       '("vendor/jemalloc-sys-0.5.4+5.3.0-patched/jemalloc"
                         "vendor/openssl-src-111.28.2+1.1.1w/openssl"
                         "vendor/tikv-jemalloc-sys-0.5.4+5.3.0-patched/jemalloc"))
             ;; Remove vendored dynamically linked libraries.
             ;; find . -not -type d -executable -exec file {} \+ | grep ELF
             ;; Also remove the bundled (mostly Windows) libraries.
             (for-each delete-file
                       (find-files "vendor" "\\.(a|dll|exe|lib)$"))
             ;; Adjust vendored dependency to explicitly use rustix with libc backend.
             (substitute* '("vendor/tempfile-3.7.1/Cargo.toml"
                            "vendor/tempfile-3.10.1/Cargo.toml")
               (("features = \\[\"fs\"" all)
                (string-append all ", \"use-libc\""))))))))))

(define-public rust-1.81
  (rust-bootstrapped-package
   rust-1.80 "1.81.0" "19yggj1qivdhf68gx2652cfi7nxjkdgy39wh7h6facpzppz4h947"))

(define-public rust-1.82
  (rust-bootstrapped-package
   rust-1.81 "1.82.0" "0ajiryki2aqsg3ydx3nfhrb5i1mmxvasfszs9qblw66skr8g8lvw"))

;;; Note: Only the latest version of Rust is supported and tested.  The
;;; intermediate rusts are built for bootstrapping purposes and should not
;;; be relied upon.  This is to ease maintenance and reduce the time
;;; required to build the full Rust bootstrap chain.
;;;
;;; Here we take the latest included Rust, make it public, and re-enable tests
;;; and extra components such as rustfm.
(define-public rust-next
  (let ((base-rust rust-1.80))
    (package
      (inherit base-rust)
      (name "rust-next")
      (properties (append
                    (alist-delete 'hidden? (package-properties base-rust))
                    (clang-compiler-cpu-architectures "17")))
      (outputs (cons* "rust-src" "tools" (package-outputs base-rust)))
      (source
       (origin
         (inherit (package-source base-rust))
         (snippet
          '(begin
             (for-each delete-file-recursively
                       '("src/llvm-project"
                         ;; "vendor/jemalloc-sys/jemalloc"
                         ;; "vendor/openssl-src/openssl"
                         ;; "vendor/tikv-jemalloc-sys/jemalloc"
                         ;; These are referenced by the cargo output
                         ;; so we unbundle them.
                         ;; "vendor/curl-sys/curl"
                         "vendor/curl-sys-0.4.72+curl-8.6.0/curl"
                         ;; "vendor/libffi-sys/libffi"
                         "vendor/libnghttp2-sys-0.1.9+1.58.0/nghttp2"
                         ;; "vendor/libz-sys/src/zlib"
                         "vendor/libz-sys-1.1.16/src/zlib"))
             ;; Use the packaged nghttp2
             (delete-file "vendor/libnghttp2-sys-0.1.9+1.58.0/build.rs")
             (with-output-to-file "vendor/libnghttp2-sys-0.1.9+1.58.0/build.rs"
               (lambda _
                 (format #t "fn main() {~@
                         println!(\"cargo:rustc-link-lib=nghttp2\");~@
                         }~%")))
             ;; Remove vendored dynamically linked libraries.
             ;; find . -not -type d -executable -exec file {} \+ | grep ELF
             ;; Also remove the bundled (mostly Windows) libraries.
             (for-each delete-file
                       (find-files "vendor" "\\.(a|dll|exe|lib)$"))
             ;; ;; Adjust vendored dependency to explicitly use rustix with libc backend.
             ;; (substitute* "vendor/tempfile/Cargo.toml"
             ;;   (("features = \\[\"fs\"" all)
             ;;    (string-append all ", \"use-libc\"")))
             ))))
      (arguments
       (substitute-keyword-arguments
         (strip-keyword-arguments '(#:tests?)
           (package-arguments base-rust))
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'unpack 'relax-gdb-auto-load-safe-path
               ;; Allow GDB to load binaries from any location, otherwise the
               ;; gdbinfo tests fail.  This is only useful when testing with a
               ;; GDB version newer than 8.2.
               (lambda _
                 (setenv "HOME" (getcwd))
                 (with-output-to-file (string-append (getenv "HOME") "/.gdbinit")
                   (lambda _
                     (format #t "set auto-load safe-path /~%")))
                 ;; Do not launch gdb with '-nx' which causes it to not execute
                 ;; any init file.
                 (substitute* "src/tools/compiletest/src/runtest.rs"
                   (("\"-nx\".as_ref\\(\\), ")
                    ""))))
             (add-after 'unpack 'disable-tests-requiring-git
               (lambda _
                 (substitute* "src/tools/cargo/tests/testsuite/git.rs"
                   ,@(make-ignore-test-list
                      '("fn fetch_downloads_with_git2_first_")))))
             (add-after 'unpack 'disable-tests-requiring-mercurial
               (lambda _
                 (with-directory-excursion "src/tools/cargo/tests/testsuite/cargo_init"
                   (substitute* '("mercurial_autodetect/mod.rs"
                                  "simple_hg_ignore_exists/mod.rs")
                     ,@(make-ignore-test-list
                        '("fn case"))))))
             (add-after 'unpack 'disable-tests-using-cargo-publish
               (lambda _
                 (with-directory-excursion "src/tools/cargo/tests/testsuite"
                   (substitute* "alt_registry.rs"
                     ,@(make-ignore-test-list
                        '("fn warn_for_unused_fields")))
                   (substitute* '("cargo_add/locked_unchanged/mod.rs"
                                  "cargo_add/lockfile_updated/mod.rs"
                                  "cargo_remove/update_lock_file/mod.rs")
                     ,@(make-ignore-test-list
                        '("fn case")))
                   (substitute* "git_shallow.rs"
                     ,@(make-ignore-test-list
                        '("fn gitoxide_clones_git_dependency_with_shallow_protocol_and_git2_is_used_for_followup_fetches"
                          "fn gitoxide_clones_registry_with_shallow_protocol_and_aborts_and_updates_again"
                          "fn gitoxide_clones_registry_with_shallow_protocol_and_follow_up_fetch_maintains_shallowness"
                          "fn gitoxide_clones_registry_with_shallow_protocol_and_follow_up_with_git2_fetch"
                          "fn gitoxide_clones_registry_without_shallow_protocol_and_follow_up_fetch_uses_shallowness"
                          "fn gitoxide_shallow_clone_followed_by_non_shallow_update"
                          "fn gitoxide_clones_shallow_two_revs_same_deps"
                          "fn gitoxide_git_dependencies_switch_from_branch_to_rev"
                          "fn shallow_deps_work_with_revisions_and_branches_mixed_on_same_dependency")))
                   (substitute* "install.rs"
                     ,@(make-ignore-test-list
                        '("fn failed_install_retains_temp_directory")))
                   (substitute* "offline.rs"
                     ,@(make-ignore-test-list
                        '("fn gitoxide_cargo_compile_offline_with_cached_git_dep_shallow_dep")))
                   (substitute* "patch.rs"
                     ,@(make-ignore-test-list
                        '("fn gitoxide_clones_shallow_old_git_patch"))))))
             ,@(if (target-riscv64?)
                   ;; Keep this phase separate so it can be adjusted without needing
                   ;; to adjust the skipped tests on other architectures.
                   `((add-after 'unpack 'disable-tests-broken-on-riscv64
                       (lambda _
                         (with-directory-excursion "src/tools/cargo/tests/testsuite"
                           (substitute* "build.rs"
                             ,@(make-ignore-test-list
                                 '("fn uplift_dwp_of_bin_on_linux")))
                           (substitute* "cache_lock.rs"
                             ,@(make-ignore-test-list
                                 '("fn multiple_shared"
                                   "fn multiple_download"
                                   "fn download_then_mutate")))
                           (substitute* "global_cache_tracker.rs"
                             ,@(make-ignore-test-list
                                 '("fn package_cache_lock_during_build")))))))
                   `())
             (add-after 'unpack 'disable-tests-broken-on-aarch64
               (lambda _
                 (with-directory-excursion "src/tools/cargo/tests/testsuite/"
                   (substitute* "build_script_extra_link_arg.rs"
                     ,@(make-ignore-test-list
                        '("fn build_script_extra_link_arg_bin_single")))
                   (substitute* "build_script.rs"
                     ,@(make-ignore-test-list
                        '("fn env_test")))
                   (substitute* "collisions.rs"
                     ,@(make-ignore-test-list
                        '("fn collision_doc_profile_split")))
                   (substitute* "concurrent.rs"
                     ,@(make-ignore-test-list
                        '("fn no_deadlock_with_git_dependencies")))
                   (substitute* "features2.rs"
                     ,@(make-ignore-test-list
                        '("fn dep_with_optional_host_deps_activated"))))))
             (add-after 'unpack 'disable-tests-failing-in-rust-next
               (lambda _
                 (with-directory-excursion "src/tools/cargo/tests/testsuite/"
                   (substitute* "check_cfg.rs"
                     ,@(make-ignore-test-list
                        '("fn config_fingerprint"
                          "fn features_fingerprint"))))))
             (add-after 'unpack 'patch-command-exec-tests
               ;; This test suite includes some tests that the stdlib's
               ;; `Command` execution properly handles in situations where
               ;; the environment or PATH variable are empty, but this fails
               ;; since we don't have `echo` available at its usual FHS
               ;; location.
               (lambda _
                 (substitute* "tests/ui/command/command-exec.rs"
                   (("Command::new\\(\"echo\"\\)")
                    (format #f "Command::new(~s)" (which "echo"))))))
             (add-after 'unpack 'patch-command-uid-gid-test
               (lambda _
                 (substitute* "tests/ui/command/command-uid-gid.rs"
                   (("/bin/sh") (which "sh"))
                   (("/bin/ls") (which "ls")))))
             (add-after 'unpack 'skip-shebang-tests
               ;; This test make sure that the parser behaves properly when a
               ;; source file starts with a shebang. Unfortunately, the
               ;; patch-shebangs phase changes the meaning of these edge-cases.
               ;; We skip the test since it's drastically unlikely Guix's
               ;; packaging will introduce a bug here.
               (lambda _
                 (delete-file "tests/ui/parser/shebang/sneaky-attrib.rs")))
             (add-after 'unpack 'patch-process-tests
               (lambda* (#:key inputs #:allow-other-keys)
                 (let ((bash (assoc-ref inputs "bash")))
                   (with-directory-excursion "library/std/src"
                     (substitute* "process/tests.rs"
                       (("\"/bin/sh\"")
                        (string-append "\"" bash "/bin/sh\"")))
                     ;; The three tests which are known to fail upstream on QEMU
                     ;; emulation on aarch64 and riscv64 also fail on x86_64 in
                     ;; Guix's build system.  Skip them on all builds.
                     (substitute* "sys/pal/unix/process/process_common/tests.rs"
                       ;; We can't use make-ignore-test-list because we will get
                       ;; build errors due to the double [ignore] block.
                       (("target_arch = \"arm\"" arm)
                        (string-append "target_os = \"linux\",\n"
                                       "        " arm)))))))
             (add-after 'unpack 'disable-interrupt-tests
               (lambda _
                 ;; This test hangs in the build container; disable it.
                 (substitute* "src/tools/cargo/tests/testsuite/freshness.rs"
                   ,@(make-ignore-test-list
                      '("fn linking_interrupted")))
                 ;; Likewise for the ctrl_c_kills_everyone test.
                 (substitute* "src/tools/cargo/tests/testsuite/death.rs"
                   ,@(make-ignore-test-list
                      '("fn ctrl_c_kills_everyone")))))
             (add-after 'unpack 'adjust-rpath-values
               ;; This adds %output:out to rpath, allowing us to install utilities in
               ;; different outputs while reusing the shared libraries.
               (lambda* (#:key outputs #:allow-other-keys)
                 (let ((out (assoc-ref outputs "out")))
                   (substitute* "src/bootstrap/src/core/builder.rs"
                     ((" = rpath.*" all)
                      (string-append all
                                     "                "
                                     "self.rustflags.arg(\"-Clink-args=-Wl,-rpath="
                                     out "/lib\");\n"))))))
             (add-after 'unpack 'unpack-profiler-rt
               ;; Copy compiler-rt sources to where libprofiler_builtins looks
               ;; for its vendored copy.
               (lambda* (#:key inputs #:allow-other-keys)
                 (mkdir-p "src/llvm-project/compiler-rt")
                 (copy-recursively
                   (string-append (assoc-ref inputs "clang-source")
                                  "/compiler-rt")
                   "src/llvm-project/compiler-rt")))
             (add-after 'configure 'enable-profiling
               (lambda _
                 (substitute* "config.toml"
                   (("^profiler =.*$") "")
                   (("\\[build\\]") "\n[build]\nprofiler = true\n"))))
             (add-after 'configure 'add-gdb-to-config
               (lambda* (#:key inputs #:allow-other-keys)
                 (let ((gdb (assoc-ref inputs "gdb")))
                   (substitute* "config.toml"
                     (("^python =.*" all)
                      (string-append all
                                     "gdb = \"" gdb "/bin/gdb\"\n"))))))
             (replace 'build
               ;; Phase overridden to also build more tools.
               (lambda* (#:key parallel-build? #:allow-other-keys)
                 (let ((job-spec (string-append
                                  "-j" (if parallel-build?
                                           (number->string (parallel-job-count))
                                           "1"))))
                   (invoke "./x.py" job-spec "build"
                           "library/std" ;rustc
                           "src/tools/cargo"
                           "src/tools/clippy"
                           "src/tools/rust-analyzer"
                           "src/tools/rustfmt"))))
             (replace 'check
               ;; Phase overridden to also test more tools.
               (lambda* (#:key tests? parallel-build? #:allow-other-keys)
                 (when tests?
                   (let ((job-spec (string-append
                                    "-j" (if parallel-build?
                                             (number->string (parallel-job-count))
                                             "1"))))
                     (invoke "./x.py" job-spec "test" "-vv"
                             "library/std"
                             "src/tools/cargo"
                             "src/tools/clippy"
                             "src/tools/rust-analyzer"
                             "src/tools/rustfmt")))))
             (replace 'install
               ;; Phase overridden to also install more tools.
               (lambda* (#:key outputs #:allow-other-keys)
                 (invoke "./x.py" "install")
                 (substitute* "config.toml"
                   ;; Adjust the prefix to the 'cargo' output.
                   (("prefix = \"[^\"]*\"")
                    (format #f "prefix = ~s" (assoc-ref outputs "cargo"))))
                 (invoke "./x.py" "install" "cargo")
                 (substitute* "config.toml"
                   ;; Adjust the prefix to the 'tools' output.
                   (("prefix = \"[^\"]*\"")
                    (format #f "prefix = ~s" (assoc-ref outputs "tools"))))
                 (invoke "./x.py" "install" "clippy")
                 (invoke "./x.py" "install" "rust-analyzer")
                 (invoke "./x.py" "install" "rustfmt")))
             (add-after 'install 'install-rust-src
               (lambda* (#:key outputs #:allow-other-keys)
                 (let ((out (assoc-ref outputs "rust-src"))
                       (dest "/lib/rustlib/src/rust"))
                   (mkdir-p (string-append out dest))
                   (copy-recursively "library" (string-append out dest "/library"))
                   (copy-recursively "src" (string-append out dest "/src")))))
             (add-after 'install 'remove-uninstall-script
               (lambda* (#:key outputs #:allow-other-keys)
                 ;; This script has no use on Guix
                 ;; and it retains a reference to the host's bash.
                 (delete-file (string-append (assoc-ref outputs "out")
                                             "/lib/rustlib/uninstall.sh"))))
             (add-after 'install-rust-src 'wrap-rust-analyzer
               (lambda* (#:key outputs #:allow-other-keys)
                 (let ((bin (string-append (assoc-ref outputs "tools") "/bin")))
                   (rename-file (string-append bin "/rust-analyzer")
                                (string-append bin "/.rust-analyzer-real"))
                   (call-with-output-file (string-append bin "/rust-analyzer")
                     (lambda (port)
                       (format port "#!~a
if test -z \"${RUST_SRC_PATH}\";then export RUST_SRC_PATH=~S;fi;
exec -a \"$0\" \"~a\" \"$@\""
                               (which "bash")
                               (string-append (assoc-ref outputs "rust-src")
                                              "/lib/rustlib/src/rust/library")
                               (string-append bin "/.rust-analyzer-real"))))
                   (chmod (string-append bin "/rust-analyzer") #o755))))))))
      (inputs
       (modify-inputs (package-inputs base-rust)
                      (prepend curl libffi `(,nghttp2 "lib") zlib)))
      (native-inputs (cons*
                      ;; Keep in sync with the llvm used to build rust.
                      `("clang-source" ,(package-source clang-runtime-15))
                      ;; Add test inputs.
                      `("gdb" ,gdb/pinned)
                      `("procps" ,procps)
                      (package-native-inputs base-rust))))))
