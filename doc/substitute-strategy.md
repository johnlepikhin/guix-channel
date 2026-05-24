# Substitute server strategy

Some channel packages take a long time to build from source:

| Package | Wall time (8 vCPU) | Disk peak | Notes |
|---------|---------------------|-----------|-------|
| `onnxruntime` 1.26 | ≈12 min | ~8 GB | LTO link is the long tail |
| `openvino` 2026.1 (CPU) | ≈30–60 min | ~10 GB | Planned (P0) |
| `intel-compute-runtime` (NEO) | ≈20–40 min | ~5 GB | Planned (P1) |
| `intel-graphics-compiler` (IGC) | **2–4 h** | **~50 GB** | Planned (P1.5); bundles LLVM fork |

Without pre-built substitutes, every `guix pull` on a new machine or a
fresh evaluation context replays those compiles.  IGC alone makes the
channel impractical to publish without substitutes — a casual user
would give up before `guix pull` returns.

## Options surveyed

1. **GitHub Actions building `.nar` archives and uploading them as
   artifacts; users add a substitute URL pointing at the latest
   release assets.**
   - Pro: free (GitHub gives 2000 free Linux minutes/month for public
     repos), no separate infra.
   - Con: artifacts time out after 90 days unless promoted to a
     Release; the substitute URL has to be the Release URL of the
     latest tag; signing keys live on the runner unless we publish
     pre-signed.
   - Con: GH Actions runners cap at 6 h per job and 20 GB temp; IGC
     could exceed the disk limit.  Workaround: use a self-hosted
     runner on the maintainer's laptop only for IGC.

2. **Self-hosted Cuirass.**
   - Pro: real Guix CI, native `.nar` publishing, signing built in.
   - Con: server upkeep, machine has to be online, postgres+nginx
     glue, certificate management.  Out of proportion for a personal
     channel.

3. **Don't publish substitutes; accept the build time.**
   - Pro: zero effort.
   - Con: blocks anyone but the maintainer from using the channel for
     `openvino`/IGC packages.  FluxFrame Stage 8 on a second machine
     would mean a 4-hour boot.

4. **Build locally, publish manually via `guix publish` on a small
   VPS.**
   - Pro: simple, predictable, can run on a $5/mo VPS.
   - Con: someone has to push.  No automation for "this package was
     bumped, rebuild & re-publish."

## Decision

**Phase 1 (now → P0/P3 land):** option 3 — no substitutes.  Channel
users (currently just the maintainer) accept ~30 min openvino build
on first install.  IGC isn't on the path yet; defer infra until P1.5
is imminent.

**IGC + NEO build resolved (May 2026):** the IRBuilderGenerator's BIF
cross-compile (`clang -target x86_64-pc-windows[-msvc]` against IGC's
bundled `clang/lib/Headers/stdint.h`) was treating `uint64_t` as 4
bytes -- somewhere in the opencl-clang patch stack applied to clang
during the IGC build the `__UINT64_TYPE__` builtin macro ends up as
`unsigned long` (4 bytes on Windows LLP64) instead of `unsigned long
long`.  Fix in `intel-graphics-compiler.scm`'s patch-source phase:
drop an `igc_bif_stdint_fix.h` next to the IGC source root with
hand-rolled 64-bit `<stdint.h>` typedefs and `#define __CLANG_STDINT_H`,
then patch `IGC/cmake/IRBuilderGeneratorCodeGen.cmake` to add
`-include ${CMAKE_SOURCE_DIR}/igc_bif_stdint_fix.h` to the
cross-compile command line.  Bundled stdint.h short-circuits via its
own guard, IR-builder kernels compile cleanly.

NEO 26.18 then required: gmmlib bumped to 22.10.0 (Panther Lake /
Xe3p / CRI / NVL enum entries), level-zero bumped to 1.28.6
(`ZE_COMMAND_LIST_FLAG_COPY_OFFLOAD_HINT`), and a `patchelf
--add-rpath` post-install phase appending `$out/lib` + IGC's `lib/`
to every binary's RUNPATH (CMAKE_INSTALL_RPATH alone didn't survive
the install rewrite).  `NEO__IGC_LIBRARY_PATH` cmake flag is now set
so the wrapped `ocloc` invocations during built-in kernel
compilation find `libigdfcl.so.2`.

Build cost on 8 cores: openvino ≈30 min, openvino-with-npu ≈35 min,
intel-graphics-compiler ≈45 min (with bundled LLVM), intel-compute-runtime
≈25 min.  All four are still infeasible without substitutes for
casual users.

**Phase 2 (before P1.5 starts):** option 1 — GitHub Actions building
`.nar` archives, posted as artifacts on a `substitutes/` release.
For IGC specifically: self-hosted runner on the maintainer's laptop
because 50 GB > 20 GB hosted-runner ceiling.  Sign the substitutes
with the channel's signing key so users can verify them.

**Phase 3 (if the channel ever has more than one human consumer):**
revisit option 4.  A $5/mo VPS running `guix publish` is the cleanest
public-facing substitute server; switching to it from Phase 2 is
mostly a DNS/`--substitute-urls` change for users.

## Concrete prerequisites for Phase 2

When P1.5 work begins, before merging the IGC package:

- [ ] Generate a channel signing keypair (`guix archive
      --generate-key`); commit the public key to `etc/keys/` so
      `make-channel-introduction` can pin it.
- [ ] Add a `release.yml` GitHub Actions workflow:
  - Trigger: `push` to `master` (or a `release-*` tag).
  - Matrix: `[onnxruntime, openvino, intel-compute-runtime]` on
    `ubuntu-latest`; IGC on the self-hosted runner.
  - Steps: `guix build PKG`, `guix archive --export PKG | gzip >
    PKG.nar.gz`, sign, upload to a GitHub Release.
- [ ] Document the substitute URL in README and `CONTRIBUTING.md`.
- [ ] In `.guix-channel`, raise `(version 1)` and pin the
      introduction.

## Out of scope for this document

* Mirror/cache server hosting (CDN, GitHub Pages, etc.) — chosen at
  Phase 2 time.
* Substitute pruning policy — let GitHub Releases handle retention
  for now.
