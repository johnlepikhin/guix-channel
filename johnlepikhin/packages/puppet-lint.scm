(define-module (johnlepikhin packages puppet-lint)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system ruby)
  #:use-module (gnu packages ruby)
  #:export (make-puppet-lint))

(define (make-puppet-lint version checksum)
  (package
    (name "puppet-lint")
    (version version)
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "puppet-lint" version))
              (sha256
               (base32 checksum))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))
    (synopsis "Puppet manifests linter")
    (description "Check that your Puppet manifest conform to the style guide")
    (home-page "http://puppet-lint.com")
    (license license:expat)))

(define-public puppet-lint-2.4.2
  (make-puppet-lint "2.4.2" "1pwpjxxr3wz71yl7jhhaa93fsrr72kz25isjydfhsf1igc9mfj9k"))

(define-public puppet-lint puppet-lint-2.4.2)

puppet-lint
