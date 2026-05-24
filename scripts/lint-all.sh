#!/bin/sh
# Run `guix lint --no-network` over every channel package, in parallel.
#
# Failure mode: exit non-zero iff any per-package lint failed with a
# `guix lint: error:` (load/parse/unknown-package).  Plain warnings
# show up in the log but do not fail the run.
#
# `--no-network` skips CVE / URL-reachability checks; those hang on
# sandboxed CI runners without delivering CI-blocking signal.
#
# Override JOBS=1 for sequential logs when debugging locally.
set -eu

cd "$(dirname "$0")/.."

CHANNEL=$(pwd)
JOBS="${JOBS:-$(nproc 2>/dev/null || echo 4)}"
TMPDIR=$(mktemp -d)
# Keep TMPDIR around for inspection when DEBUG=1; otherwise clean up.
[ "${DEBUG:-0}" = 1 ] || trap 'rm -rf "$TMPDIR"' EXIT

./scripts/list-packages.sh > "$TMPDIR/pkgs"

# Run lint per package in parallel; each subshell writes its full
# output to $TMPDIR/<pkg>.log and its exit code to $TMPDIR/<pkg>.rc.
xargs -a "$TMPDIR/pkgs" -P "$JOBS" -I {} sh -c '
    pkg=$1
    log="$2/$pkg.log"
    rc="$2/$pkg.rc"
    # Close stdin so `guix lint` (which itself reads stdin when no
    # package args are present after parsing some flag combinations)
    # does not consume the pkgs file that xargs is iterating over.
    guix lint --no-network -L "$3" "$pkg" < /dev/null > "$log" 2>&1
    echo $? > "$rc"
' _ {} "$TMPDIR" "$CHANNEL"

# Replay logs in alphabetical order.  Mark failures.
status=0
for log in "$TMPDIR"/*.log; do
    pkg=$(basename "$log" .log)
    rc=$(cat "${log%.log}.rc")
    if [ "$rc" -eq 0 ]; then
        printf '=== %s ===\n' "$pkg"
    else
        printf '=== %s [FAIL] ===\n' "$pkg"
        status=1
    fi
    cat "$log"
done

if [ "$status" -ne 0 ]; then
    printf '\n=== Failed packages ===\n' >&2
    for log in "$TMPDIR"/*.log; do
        rc=$(cat "${log%.log}.rc")
        [ "$rc" -ne 0 ] && basename "$log" .log >&2
    done
fi

exit "$status"
