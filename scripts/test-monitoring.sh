#!/bin/sh
# Tests of the monitoring services: the alert rule libraries against the
# unit tests in scripts/test-monitoring/rules-test.yml, and the output of the
# textfile collectors on recorded command output.
#
# Usage: scripts/test-monitoring.sh [SSH-HOST]
#
# promtool comes from the Prometheus image the service uses.  It runs in a
# local docker or podman, or, given SSH-HOST, in the podman or docker of
# that host.

set -eu

channel_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
tests="$channel_dir/scripts/test-monitoring"
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT INT TERM

image=$(sed -n 's/^  "\(quay.io\/prometheus\/prometheus:[^"]*\)")$/\1/p' \
            "$channel_dir/johnlepikhin/system/services/monitoring.scm")
[ -n "$image" ] || { echo "no Prometheus image found" >&2; exit 1; }

guix repl -L "$channel_dir" -- "$tests/generate.guile" "$work"
cp "$tests/rules-test.yml" "$work/"

# The checks, run in a directory holding the generated files.
checks='set -e
runtime=$(command -v podman || command -v docker)
# promtool runs as nobody.
chmod -R a+rX .
promtool() {
    "$runtime" run --rm -i -v "$PWD:/t:ro" -w /t --entrypoint /bin/promtool "$0" "$@"
}
promtool check rules rules.yml
promtool test rules rules-test.yml
for metrics in shepherd.prom smart.prom restic-freshness.prom; do
    echo "Checking $metrics"
    promtool check metrics < "$metrics"
done'

if [ $# -eq 0 ]; then
    (cd "$work" && sh -c "$checks" "$image")
else
    remote=$(ssh "$1" mktemp -d)
    tar -C "$work" -cf - . | ssh "$1" "tar -C '$remote' -xf -"
    status=0
    ssh "$1" "cd '$remote' && sh -c '$(printf %s "$checks" | sed "s/'/'\\\\''/g")' '$image'" \
        || status=$?
    ssh "$1" "rm -rf '$remote'"
    exit $status
fi
