#!/bin/sh
# Print every channel package's (package-name) value, one per line.
#
# The Scheme program is materialised into a tempfile *outside* the
# channel root and then passed to `guix repl -- FILE` (script mode,
# no REPL banner).  We deliberately do not keep the Scheme as a
# permanent `.scm` file inside the channel: `guix lint -L
# <channel-root>` auto-loads every `.scm` reachable from the load
# path, and a stray helper with top-level forms then runs as a side
# effect of every lint invocation and corrupts its output.
#
# Matching .scm filename to package name is unreliable (ai.scm →
# claude-code, oping.scm → liboping, one-password.scm → "1password"),
# which is why we ask the package records themselves for the name.
set -eu

cd "$(dirname "$0")/.."

CHANNEL=$(pwd)
SCRIPT=$(mktemp --suffix=.scm)
trap 'rm -f "$SCRIPT"' EXIT

cat > "$SCRIPT" <<'SCHEME'
(use-modules (guix packages)
             (ice-9 ftw)
             (srfi srfi-1))

(define %channel-root (getenv "CHANNEL_ROOT"))
(define %packages-dir (string-append %channel-root "/johnlepikhin/packages"))

;; Modules that don't define end-user packages: crate-source databases
;; (rust-crates), build/version helpers (rust*, python-xyz).  Excluded
;; from the list so CI doesn't waste time linting them.
(define excluded-modules
  '((johnlepikhin packages rust-crates)
    (johnlepikhin packages rust)
    (johnlepikhin packages rust-binary)
    (johnlepikhin packages rust-nightly)
    (johnlepikhin packages python-xyz)))

(define (scm->module file)
  `(johnlepikhin packages ,(string->symbol (basename file ".scm"))))

(define (module-packages mod)
  (let ((iface (resolve-interface mod))
        (out '()))
    (module-for-each
     (lambda (_ var)
       (when (variable-bound? var)
         (let ((v (variable-ref var)))
           (when (package? v)
             (set! out (cons v out))))))
     iface)
    out))

(define mods
  (remove (lambda (m) (member m excluded-modules))
          (map scm->module
               (scandir %packages-dir
                        (lambda (n) (string-suffix? ".scm" n))))))

(for-each (lambda (p) (format #t "~a~%" (package-name p)))
          (delete-duplicates
           (sort (append-map module-packages mods)
                 (lambda (a b) (string<? (package-name a) (package-name b))))
           (lambda (a b) (string=? (package-name a) (package-name b)))))
SCHEME

CHANNEL_ROOT="$CHANNEL" guix repl -L "$CHANNEL" -- "$SCRIPT" 2>/dev/null
