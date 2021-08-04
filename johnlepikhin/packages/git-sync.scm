(define-module (johnlepikhin packages git-sync)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (gnu packages version-control)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:export (make-git-sync))

(define (make-git-sync version commit checksum)
  (package
    (name "git-sync")
    (version version)
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/simonthum/git-sync/")
                    (commit commit)))
             (file-name (git-file-name name version))
             (sha256
              (base32 checksum))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan `(("git-sync" "bin/git-sync"))))
    (synopsis "Synchronize tracking repositories")
    (description "This scrips intends to sync near-automatically via git in \"tracking\" repositories where a nice history is not as crucial
as having one.")
    (home-page "https://github.com/simonthum/git-sync/")
    (propagated-inputs `(("git" ,git)))
    (license cc0)))

(define-public git-sync-2021-07-27
  (make-git-sync "2020-11-09" "12e0b753055f5d7ccb904b556db982ccf7e8aa84" "0wrwmh852a2xjpzsd45fmpg9v1k20fwy5dl7cs5lc5c6k4mhigbi"))

(define-public git-sync git-sync-2021-08-04)
