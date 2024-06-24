
(define-module (johnlepikhin packages ansible-lint)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-build)
  #:use-module (guix utils)
  #:use-module (guix gexp))

(define-public python-bracex
  (package
    (name "python-bracex")
    (version "2.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "bracex" version))
       (sha256
        (base32 "1ssb7339p60p3pfzmqc1dahix78jvwzqyylbspz63x9cyhfsyzm2"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #false))
    (propagated-inputs (list python-importlib-resources python-hatchling))
    (home-page "https://github.com/facelessuser/bracex")
    (synopsis "Bracex is a brace expanding library (à la Bash) for Python.")
    (description
     "Bracex is a brace expanding library (à la Bash) for Python. Brace expanding is used to generate arbitrary strings.")
    (license license:bsd-3)))

(define-public python-subprocess-tee
  (package
    (name "python-subprocess-tee")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "subprocess_tee" version))
       (sha256
        (base32 "14358xqjl4mrm5lswbhdkkyz9qxy1bmg5jjav24716mf7bdb9cli"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #false))
    (propagated-inputs (list python-importlib-resources python-hatchling))
    (home-page "https://github.com/pycontribs/subprocess-tee")
    (synopsis "This package provides a drop-in alternative to subprocess.run.")
    (description
     "This package provides a drop-in alternative to subprocess.run that captures the output while still printing it in
real-time, just the way tee does.

Printing output in real-time while still capturing is valuable for any tool that executes long-running child processes.
 For those, you do want to provide instant feedback (progress) related to what is happening.")
    (license license:bsd-3)))

(define-public python-wcmatch
  (package
    (name "python-wcmatch")
    (version "8.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "wcmatch" version))
       (sha256
        (base32 "1clygf42hc8qw8aggy6nic9nqx8hihkp6yyq5lwgp0padnw240m7"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      '(let ((disabled-tests (list "test_tilde"
                                   "test_tilde_bytes"
                                   "test_glob_match_real_outside_curdir"
                                   "test_tilde_globmatch"
                                   "test_tilde_globmatch_no_realpath"
                                   "test_tilde_globmatch_no_tilde")))
         (list "-k" (string-append "not "
                                   (string-join disabled-tests
                                                " and not "))))))
    (propagated-inputs (list python-importlib-resources python-hatchling python-bracex))
    (native-inputs
     (list cmake pybind11 python-pytest))
    (home-page "https://github.com/facelessuser/wcmatch")
    (synopsis "Wildcard Match provides an enhanced fnmatch, glob, and pathlib library.")
    (description
     "Wildcard Match provides an enhanced fnmatch, glob, and pathlib library in order to provide file matching and
 globbing that more closely follows the features found in Bash. In some ways these libraries are similar to Python's
builtin libraries as they provide a similar interface to match, filter, and glob the file system. But they also include
a number of features found in Bash's globbing such as backslash escaping, brace expansion, extended glob pattern groups,
etc. They also add a number of new useful functions as well, such as globmatch which functions like fnmatch, but for
paths.")
    (license license:bsd-3)))

(define-public ansible-lint
  (package
    (name "ansible-lint")
    (version "24.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ansible/ansible-lint")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1k9zinpb3z6adw7jnnw8gz7rxk0h9hwww8h1xx57999ca3608xmf"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #false
      #:phases
      #~(modify-phases %standard-phases
          (delete 'sanity-check))))
    (inputs
     (list ))
    (native-inputs
     (list python))
    (propagated-inputs
     (list python-wcmatch python-yamllint python-subprocess-tee))
    (synopsis "Checks playbooks for practices and behaviour that could potentially be improved")
    (description
     "Ansible-lint checks playbooks for practices and behaviour that could potentially be improved. It is not a
 style-checker; if you want a linter for Ansible, that's ansible-lint. It checks for practices and behaviour that could
potentially be improved. This could be things such as: Deprecated features, Behaviour that could potentially be
improved, Syntax or logic")
    (license license:gpl3)
    (home-page "https://github.com/ansible/ansible-lint")))
