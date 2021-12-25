
(define-module (johnlepikhin packages perl)
  #:use-module (guix licenses)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system perl)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages language)
  #:use-module (gnu packages perl-check)
  #:export (perl-string-format
            perlcritic
            perl-pod-spell
            perl-ppix-quotelike
            perl-ppix-regexp
            perl-ppix-utilities
            perl-config-tiny))

(define-public perl-string-format
  (package
    (name "perl-string-format")
    (version "1.18")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://cpan/authors/id/S/SR/SREZIC/String-Format-"
                            version ".tar.gz"))
        (sha256
         (base32 "0y77frxzjifd4sw0j19cc346ysas1mya84rdxaz279lyin7plhcy"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-test-more" ,perl-module-build)))
    (home-page "https://metacpan.org/pod/String::Format")
    (synopsis "sprintf-like string formatting capabilities with arbitrary format definitions")
    (description "String::Format - sprintf-like string formatting capabilities with arbitrary format definitions")
    (license perl-license)))

(define-public perl-config-tiny
  (package
    (name "perl-config-tiny")
    (version "2.26")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://cpan/authors/id/R/RS/RSAVAGE/Config-Tiny-"
                            version ".tgz"))
        (sha256
         (base32 "0pyggn3yq9ffjnw3i1n5r9kg4b90jw926apbvzxq8y7cpa8k5dc3"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-file-temp" ,perl-file-temp)))
    (propagated-inputs
     `(("perl-file-temp" ,perl-file-temp)))
    (home-page "https://metacpan.org/pod/Config::Tiny")
    (synopsis "Read/Write .ini style files with as little code as possible")
    (description "Config::Tiny is a Perl class to read and write .ini style configuration files with as little code as possible, reducing
load time and memory overhead.")
    (license perl-license)))

(define-public perl-ppix-utilities
  (package
    (name "perl-ppix-utilities")
    (version "1.001000")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://cpan/authors/id/E/EL/ELLIOTJS/PPIx-Utilities-"
                            version ".tar.gz"))
        (sha256
         (base32 "16yb7dnz8lgq2azs8jxj1wac60kbn16x8y4py04ci8nndww87903"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-exception-class" ,perl-exception-class)
       ("perl-ppi" ,perl-ppi)
       ("perl-readonly" ,perl-readonly)
       ("perl-module-build" ,perl-module-build)
       ("perl-test-deep" ,perl-test-deep)))
    (propagated-inputs
     `(("perl-exception-class" ,perl-exception-class)
       ("perl-ppi" ,perl-ppi)
       ("perl-readonly" ,perl-readonly)))
    (home-page "https://metacpan.org/pod/PPIx::Utilities")
    (synopsis "This module does nothing but act as a handle for the PPIx-Utilities distribution.")
    (description "This is a collection of functions for dealing with PPI objects, many of which originated in Perl::Critic. They are
organized into modules by the kind of PPI class they relate to, by replacing the \"PPI\" at the front of the module name
with \"PPIx::Utilities\", e.g. functionality related to PPI::Nodes is in PPIx::Utilities::Node.")
    (license perl-license)))

(define-public perl-ppix-regexp
  (package
    (name "perl-ppix-regexp")
    (version "0.080")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://cpan/authors/id/W/WY/WYANT/PPIx-Regexp-"
                            version ".tar.gz"))
        (sha256
         (base32 "1y1mvdxg1sgjzlqvrk4fs5k40fzfx4jszdlynn4m3qm1g0nbwk01"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-ppi" ,perl-ppi)
       ("perl-module-build" ,perl-module-build)))
    (propagated-inputs
     `(("perl-ppi" ,perl-ppi)))
    (home-page "https://metacpan.org/pod/PPIx::Regexp")
    (synopsis "Represent a regular expression of some sort")
    (description "Represent a regular expression of some sort")
    (license perl-license)))

(define-public perl-ppix-quotelike
  (package
    (name "perl-ppix-quotelike")
    (version "0.017")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://cpan/authors/id/W/WY/WYANT/PPIx-QuoteLike-"
                            version ".tar.gz"))
        (sha256
         (base32 "1swb8gd189ah026i597xx8ssnbgd9bxhn7xcpyhwrpk8hi09mdrk"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-ppi" ,perl-ppi)
       ("perl-module-build" ,perl-module-build)
       ("perl-readonly" ,perl-readonly)))
    (propagated-inputs
     `(("perl-ppi" ,perl-ppi)
       ("perl-readonly" ,perl-readonly)))
    (home-page "https://metacpan.org/pod/PPIx::QuoteLike")
    (synopsis "Parse Perl string literals and string-literal-like things.")
    (description "This Perl class parses Perl string literals and things that are reasonably like string literals. Its real reason for being
is to find interpolated variables for Perl::Critic policies and similar code.")
    (license perl-license)))

(define-public perl-pod-spell
  (package
    (name "perl-pod-spell")
    (version "1.20")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://cpan/authors/id/D/DO/DOLMEN/Pod-Spell-"
                            version ".tar.gz"))
        (sha256
         (base32 "0g6hdnc98gc3widr7sja313b1606g37a0mw0l0wxih1bwazzg0v3"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-file-sharedir-install" ,perl-file-sharedir-install)
       ("perl-file-sharedir" ,perl-file-sharedir)
       ("perl-test-deep" ,perl-test-deep)
       ("perl-class-tiny" ,perl-class-tiny)
       ("perl-path-tiny" ,perl-path-tiny)
       ("perl-pod-parser" ,perl-pod-parser)
       ("perl-lingua-en-inflect" ,perl-lingua-en-inflect)))
    (propagated-inputs
     `(("perl-class-tiny" ,perl-class-tiny)
       ("perl-file-sharedir" ,perl-file-sharedir)
       ("perl-lingua-en-inflect" ,perl-lingua-en-inflect)
       ("perl-path-tiny" ,perl-path-tiny)))
    (home-page "https://metacpan.org/pod/Pod::Spell")
    (synopsis "A formatter for spellchecking Pod")
    (description "A formatter for spellchecking Pod")
    (license perl-license)))

(define-public perl-perlcritic
  (package
    (name "perl-perlcritic")
    (version "1.140")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://cpan/authors/id/P/PE/PETDANCE/Perl-Critic-"
                            version ".tar.gz"))
        (sha256
         (base32 "1nzxpn71mrpp85yxrxlraj52q2skvf9ja887ls11d57h6smg1vmz"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-module-build" ,perl-module-build)
       ("perl-module-pluggable" ,perl-module-pluggable)
       ("perl-b-keywords" ,perl-b-keywords)
       ("perl-list-moreutils" ,perl-list-moreutils)
       ("perl-ppi" ,perl-ppi)
       ("perl-ppix-utilities" ,perl-ppix-utilities)
       ("perl-ppix-regexp" ,perl-ppix-regexp)
       ("perl-ppix-quotelike" ,perl-ppix-quotelike)
       ("perl-test-deep" ,perl-test-deep)
       ("perl-readonly" ,perl-readonly)
       ("perl-exception-class" ,perl-exception-class)
       ("perl-string-format" ,perl-string-format)
       ("perl-config-tiny" ,perl-config-tiny)
       ("perl-pod-spell" ,perl-pod-spell)
       ("perl-pod-parser" ,perl-pod-parser)
       ("perl-file-which" ,perl-file-which)
       ("perltidy" ,perltidy)))
    (propagated-inputs
     `(("perl-readonly" ,perl-readonly)
       ("perl-module-pluggable" ,perl-module-pluggable)
       ("perl-b-keywords" ,perl-b-keywords)
       ("perl-list-moreutils" ,perl-list-moreutils)
       ("perl-ppi" ,perl-ppi)
       ("perl-ppix-utilities" ,perl-ppix-utilities)
       ("perl-ppix-regexp" ,perl-ppix-regexp)
       ("perl-ppix-quotelike" ,perl-ppix-quotelike)
       ("perl-exception-class" ,perl-exception-class)
       ("perl-string-format" ,perl-string-format)
       ("perl-config-tiny" ,perl-config-tiny)
       ("perl-pod-spell" ,perl-pod-spell)
       ("perl-pod-parser" ,perl-pod-parser)
       ("perl-file-which" ,perl-file-which)
       ("perltidy" ,perltidy)))
    (home-page "https://metacpan.org/dist/Perl-Critic")
    (synopsis "Command-line interface to critique Perl source.")
    (description "perlcritic is a Perl source code analyzer. It is the executable front-end to the Perl::Critic engine, which attempts to
identify awkward, hard to read, error-prone, or unconventional constructs in your code. Most of the rules are based on Damian Conway's book
Perl Best Practices. However, perlcritic is not limited to enforcing PBP, and it will even support rules that contradict Conway. All rules
can easily be configured or disabled to your liking.")
    (license perl-license)))
