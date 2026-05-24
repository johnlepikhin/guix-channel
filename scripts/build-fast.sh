#!/bin/sh
# Build all "fast" packages in the channel via `guix build --dry-run`.
#
# Dry-run mode: we verify the derivation graph evaluates (catches every
# kind of input-resolution and gexp-construction error) without actually
# fetching/compiling sources.  Real builds for heavyweights (onnxruntime,
# openvino, intel-graphics-compiler...) belong on the substitute server.
set -eu

cd "$(dirname "$0")/.."

CHANNEL=$(pwd)
status=0

for pkg in $(./scripts/list-packages.sh); do
    printf '=== %s ===\n' "$pkg"
    if ! guix build -L "$CHANNEL" --dry-run "$pkg" >/dev/null 2>&1; then
        printf '!!! derivation failed for %s\n' "$pkg" >&2
        guix build -L "$CHANNEL" --dry-run "$pkg" 2>&1 | tail -20 >&2
        status=1
    fi
done

exit "$status"
