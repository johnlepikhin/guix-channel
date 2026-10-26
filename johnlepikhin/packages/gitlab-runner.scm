;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Evgenii Lepikhin <johnlepikhin@gmail.com>
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

(define-module (johnlepikhin packages gitlab-runner)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy))

;; The upstream release binary is used instead of a source build: the runner
;; pulls in several hundred Go modules that are not packaged in Guix.  The
;; binary is statically linked, so it needs no patching.
(define-public gitlab-runner
  (package
    (name "gitlab-runner")
    (version "19.5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://gitlab-runner-downloads.s3.amazonaws.com/v"
                    version "/binaries/gitlab-runner-linux-amd64"))
              (file-name (string-append name "-" version))
              (sha256
               (base32
                "11fvbswg1kh41kv146n91xg85qn8gqwyx6i6hv96fbim0haff48f"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~`((,#$(string-append name "-" version) "bin/gitlab-runner"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'make-executable
            (lambda _
              (chmod (string-append #$output "/bin/gitlab-runner") #o555))))))
    (supported-systems '("x86_64-linux"))
    (home-page "https://docs.gitlab.com/runner/")
    (synopsis "Agent that runs GitLab CI/CD jobs")
    (description
     "GitLab Runner picks up CI/CD jobs from a GitLab instance and runs them
with one of several executors (shell, Docker, Kubernetes and others), sending
the results back to GitLab.")
    (license license:expat)))
