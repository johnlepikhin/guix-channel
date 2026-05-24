# Contributing

Personal Guix channel: extra packages and system services for
`johnlepikhin`. This file is for human contributors (and future-me) —
it documents the workflow, the conventions, and the gotchas that took
real time to learn.

## Layout

```
johnlepikhin/
  packages/        ← all packages.  One module per concern, file name
                     is informational only (the public symbol need not
                     match — see `list-packages` heuristic).
  system/          ← operating-system glue
    services.scm   ← `make-system-services` that consumers wire into
                     their `operating-system` definition.
    services/      ← one shepherd/service-type per file (brightnessctl,
                     accel, fprintd, throttled, …).
  home/            ← Guix Home services (one file per service)
  devel/           ← convenience: meta-packages bundling dev tools.

scripts/           ← lint/build helpers, not part of the channel
                     surface.  See "Why scripts/ are tricky" below.

.github/workflows/ ← CI
```

## Adding a new package

1. Drop the file under `johnlepikhin/packages/<name>.scm`.  Module
   header is GPL v3+ with
   `Copyright © <year> Evgenii Lepikhin <johnlepikhin@gmail.com>`.
2. `define-module` is `(johnlepikhin packages <basename>)`.
3. Use existing precedents:
   * Rust binary release tarball with `patchelf` → `rmpc.scm`,
     `rpm-spec-tool.scm`.
   * Rust source build via `cargo-build-system` + `rust-crates.scm` →
     `ast-index.scm`.  Add new crate sources to `rust-crates.scm`
     under the per-package `cargo-inputs` block.
   * CMake source build with heavy upstream FetchContent rewriting →
     `onnxruntime.scm`.  Pattern: replace `'relax-dependencies` phase
     to substitute `cmake/external/*.cmake` and force system libs.
   * Pre-built binary (nonfree app) → `google-chrome.scm`, `zoom.scm`.
4. Before opening a PR / pushing:
   * `guix lint -L $(pwd) <pkg-name>` — must exit clean (warnings ok,
     errors not).
   * `guix build -L $(pwd) <pkg-name>` — must produce a store path.
   * `./scripts/lint-all.sh` — full-channel sanity (≈3 min).
5. Commit message:
   * `feat: Package <name> <version>` — new package.
   * `feat: Update <name> to <version>` — version bump.
   * `fix: <name>: <one-line summary>` — bugfix.

## Bumping a version

* For Rust crates with cargo-build-system: regenerate
  `(rust-crates.scm)` entries — `guix import crate <name>@<ver>
  --recursive` is the usual workflow.
* For prebuilt-binary packages: get the new tarball/whl, compute
  `guix hash <file>` (or `guix hash -rx --serializer=nar <dir>` for a
  recursively-hashed tree), bump `version` and `sha256`.  Verify with
  `guix build`.
* For inherited packages (`onnxruntime` overriding the upstream Guix
  one): bump `version`, fetch fresh source hash, re-evaluate the
  `relax-dependencies`/inputs deltas against the new upstream tree.

## Why `scripts/` are tricky

`guix lint -L <channel-root>` auto-walks every `.scm` under the load
path.  A stray `scripts/foo.scm` with top-level forms will *run as a
side effect* of `guix lint` and corrupt its output.  Workaround used
by `scripts/list-packages.sh`: emit the Scheme into a temp file
*outside* the channel and feed it to `guix repl -- /tmp/...`.

If you need a new helper that's actual Scheme, either:

* keep it as a shell wrapper around a tempfile (current pattern), or
* convert it to a `define-module` and instantiate it from a small
  driver — but be aware that top-level forms in a `(define-module ...)`
  file still run when something `(use-modules ...)` it.

## CI

GitHub Actions (`.github/workflows/ci.yml`) installs Ubuntu's `guix`
package and runs `scripts/lint-all.sh` in parallel on each push / PR.
Real `guix build`s are *not* run in CI — heavyweights (`openvino`,
`onnxruntime`, `intel-graphics-compiler`) need substitutes the CI can't
provide.  The build matrix happens on the maintainer's laptop and is
published via the channel's substitute server (see the OpenVINO plan
in `~/.claude/plans/`).

## Recurring patterns to know

* **Rust C compiler shim**: cargo's build.rs scripts call `cc` (no
  prefix), Guix doesn't put `gcc-toolchain/bin/gcc` on the path under
  that name.  The pattern duplicated in several packages
  (`ast-index.scm`, `clio.scm`, `voice-type.scm`, …) drops a symlink
  in a build-local bin dir and exports `PATH`/`CC`/`HOST_CC`.  Slated
  for extraction into a shared helper.
* **`cargo-inputs` databases**: `rust-crates.scm` holds per-package
  lists of `crate-source` records.  Each new Rust package gets its
  own `(<pkg> => (list rust-aho-corasick-... ...))` block at the
  bottom.
* **udev rules and device groups**: see `johnlepikhin/system/services/accel.scm`
  for the canonical pattern (custom group via the `#:groups` arg of
  `udev-rules-service`, no manual `user-group` plumbing on the
  operating-system side).
* **nonfree licenses**: import via
  `(use-modules ((nonguix licenses) #:prefix nonguix-license:))` and
  use `(nonguix-license:nonfree "<url>")`.  Blueprint:
  `intel-microcode` in nonguix's `nongnu/packages/linux.scm`.

## Private system config

User-specific values (concrete IPs, hostnames, secrets, specific user
account members) live in `~/.config/guix/private/`, never in this
public channel.  See `MEMORY.md` policy.
