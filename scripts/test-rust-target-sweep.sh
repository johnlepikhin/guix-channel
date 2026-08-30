#!/bin/sh
# Fixture-based tests for the rust-target-sweep home service.
#
# The classification barriers are the whole point of that service: a plain
# `find -name target' also matches a libvirt runtime directory, the contents
# of an npm package and a Rust source module called `target'.  This script
# builds a tree containing every case that has been seen in the wild, runs the
# real sweeper against it, and compares verdicts and survivors.
#
# Usage: scripts/test-rust-target-sweep.sh [path/to/rust-target-sweep]
#
# With no argument the script builds the sweeper from the channel checkout it
# lives in.

set -eu

channel_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
work=$(mktemp -d)
failures=0
checks=0

cleanup() {
    # A fixture deliberately contains an unreadable directory.
    chmod -R u+rwX "$work" 2>/dev/null || true
    rm -rf "$work"
}
trap cleanup EXIT INT TERM

say() { printf '%s\n' "$*"; }

ok() {
    checks=$((checks + 1))
    printf '  ok    %s\n' "$1"
}

fail() {
    checks=$((checks + 1))
    failures=$((failures + 1))
    printf '  FAIL  %s\n' "$1"
    [ $# -gt 1 ] && printf '        %s\n' "$2"
    return 0
}

check_exists() {
    if [ -e "$1" ]; then ok "$2"; else fail "$2" "нет: $1"; fi
}

check_absent() {
    if [ -e "$1" ]; then fail "$2" "осталось: $1"; else ok "$2"; fi
}

check_eq() {
    if [ "$1" = "$2" ]; then ok "$3"; else fail "$3" "ожидалось [$2], получено [$1]"; fi
}

# ---------------------------------------------------------------- sweeper

if [ $# -ge 1 ]; then
    sweeper=$1
else
    say "Собираю rust-target-sweep из $channel_dir ..."
    sweeper=$(guix build -L "$channel_dir" -e '(begin
  (use-modules (johnlepikhin home rust-target-sweep) (guix gexp))
  ((@@ (johnlepikhin home rust-target-sweep) sweep-script)
   (home-rust-target-sweep-configuration)))' | tail -1)
fi
say "sweeper: $sweeper"

# The sweeper keeps its journal and its global lock under XDG_STATE_HOME;
# point both at the scratch tree so a test run never touches the real one.
XDG_STATE_HOME="$work/state"
export XDG_STATE_HOME
mkdir -p "$XDG_STATE_HOME"

fix="$work/fixture"
old=$(date -d '-200 days' '+%Y-%m-%d %H:%M:%S')
recent=$(date -d '-1 day' '+%Y-%m-%d %H:%M:%S')

sweep() {
    "$sweeper" --root "$fix" "$@" >"$work/out" 2>"$work/err"
}

# ---------------------------------------------------------------- fixtures

# A cargo layout is recognised by .fingerprint or .cargo-lock inside it.
make_layout() {
    layout=$1; age=$2
    mkdir -p "$layout/.fingerprint" "$layout/deps" "$layout/incremental"
    : > "$layout/.cargo-lock"
    : > "$layout/deps/libthing.rlib"
    touch -d "$age" "$layout/deps/libthing.rlib" "$layout/.cargo-lock"
    touch -d "$age" "$layout/.fingerprint" "$layout/deps"
}

make_tag() {
    printf 'Signature: 8a477f597d28d172789f06886806bc55\n# cargo\n' \
        > "$1/CACHEDIR.TAG"
}

build_fixture() {
    rm -rf "$fix"

    # 1. Ordinary live project: stale and fresh incremental side by side,
    #    plus artefacts that must survive rule B.
    mkdir -p "$fix/live/src" "$fix/live/target"
    : > "$fix/live/Cargo.toml"
    make_tag "$fix/live/target"
    : > "$fix/live/target/.rustc_info.json"
    make_layout "$fix/live/target/debug" "$recent"
    mkdir -p "$fix/live/target/debug/incremental/crate-stale" \
             "$fix/live/target/debug/incremental/crate-fresh" \
             "$fix/live/target/package" "$fix/live/target/debian" \
             "$fix/live/target/doc"
    : > "$fix/live/target/debug/incremental/crate-stale/s-marker"
    : > "$fix/live/target/debug/incremental/crate-fresh/s-marker"
    : > "$fix/live/target/package/live-0.1.0.crate"
    : > "$fix/live/target/debian/live_0.1.0_amd64.deb"
    : > "$fix/live/target/doc/index.html"
    touch -d "$old" "$fix/live/target/debug/incremental/crate-stale"
    touch -d "$recent" "$fix/live/target/debug/incremental/crate-fresh"

    # 2. Built by cargo < 1.37: no CACHEDIR.TAG, only .rustc_info.json.
    mkdir -p "$fix/oldcargo/target"
    : > "$fix/oldcargo/Cargo.toml"
    : > "$fix/oldcargo/target/.rustc_info.json"
    make_layout "$fix/oldcargo/target/debug" "$recent"

    # 3. Cargo fingerprints but no manifest beside them: not our business.
    mkdir -p "$fix/nomanifest/target"
    make_tag "$fix/nomanifest/target"
    make_layout "$fix/nomanifest/target/debug" "$recent"

    # 4. A manifest, but `target' holds nothing cargo ever wrote.
    mkdir -p "$fix/nofingerprint/target/whatever"
    : > "$fix/nofingerprint/Cargo.toml"

    # 5. The libvirt case: a runtime directory that happens to be `target'.
    mkdir -p "$fix/libvirt/qemu/channel/target/domain-1"

    # 6. Inside node_modules: pruned before classification.
    mkdir -p "$fix/web/node_modules/pkg/target/debug/.fingerprint"
    : > "$fix/web/node_modules/pkg/Cargo.toml"

    # 7. Rust *source* module named target, inside a real project.
    mkdir -p "$fix/srcmod/src/commands/target"
    : > "$fix/srcmod/Cargo.toml"
    : > "$fix/srcmod/src/commands/target/mod.rs"

    # 8. A symlink pointing out of the tree.
    mkdir -p "$fix/symlinked" "$work/outside"
    : > "$fix/symlinked/Cargo.toml"
    ln -s "$work/outside" "$fix/symlinked/target"

    # 9. Cross-compiled layout: one level deeper than the plain one.
    mkdir -p "$fix/cross/target"
    : > "$fix/cross/Cargo.toml"
    make_tag "$fix/cross/target"
    make_layout "$fix/cross/target/aarch64-unknown-linux-gnu/release" "$recent"
    mkdir -p "$fix/cross/target/aarch64-unknown-linux-gnu/release/incremental/crate-stale"
    : > "$fix/cross/target/aarch64-unknown-linux-gnu/release/incremental/crate-stale/s-marker"
    touch -d "$old" "$fix/cross/target/aarch64-unknown-linux-gnu/release/incremental/crate-stale"

    # 10. Dormant project holding published artefacts.  This is the case that
    #     the naive "delete the whole target" rule destroys.
    mkdir -p "$fix/dormant/target/package" "$fix/dormant/target/debian"
    : > "$fix/dormant/Cargo.toml"
    make_tag "$fix/dormant/target"
    : > "$fix/dormant/target/.rustc_info.json"
    make_layout "$fix/dormant/target/debug" "$old"
    make_layout "$fix/dormant/target/release" "$old"
    : > "$fix/dormant/target/package/dormant-0.1.0.crate"
    : > "$fix/dormant/target/debian/dormant_0.1.0_amd64.deb"
    touch -d "$old" "$fix/dormant/target/package/dormant-0.1.0.crate" \
             "$fix/dormant/target/debian/dormant_0.1.0_amd64.deb" \
             "$fix/dormant/target/.rustc_info.json"

    # 11. Empty incremental directory: nothing to do, no crash.
    mkdir -p "$fix/emptyinc/target"
    : > "$fix/emptyinc/Cargo.toml"
    make_tag "$fix/emptyinc/target"
    make_layout "$fix/emptyinc/target/debug" "$recent"

    # 12. Workspace member with its own manifest but no output of its own.
    mkdir -p "$fix/workspace/member/src" "$fix/workspace/target"
    : > "$fix/workspace/Cargo.toml"
    : > "$fix/workspace/member/Cargo.toml"
    make_tag "$fix/workspace/target"
    make_layout "$fix/workspace/target/debug" "$recent"

    # 13. Unreadable subdirectory: the walk must survive EACCES.
    mkdir -p "$fix/noperm/target"
    : > "$fix/noperm/Cargo.toml"
    make_tag "$fix/noperm/target"
    make_layout "$fix/noperm/target/debug" "$recent"
    mkdir -p "$fix/noperm/target/debug/sealed"
    chmod 000 "$fix/noperm/target/debug/sealed"
}

verdict_for() {
    awk -F'\t' -v path="$1" '$2 == path {print $1 " " $3}' "$work/out"
}

# ---------------------------------------------------------------- 1. verdicts

build_fixture
say ""
say "1. Классификация (--print-targets)"
sweep --print-targets

for path in "$fix/live/target" "$fix/oldcargo/target" "$fix/cross/target" \
            "$fix/dormant/target" "$fix/emptyinc/target" \
            "$fix/workspace/target" "$fix/noperm/target"; do
    case "$(verdict_for "$path")" in
        ACCEPT*) ok "ACCEPT $path" ;;
        *)       fail "ACCEPT $path" "получено: $(verdict_for "$path")" ;;
    esac
done

for path in "$fix/nomanifest/target" "$fix/nofingerprint/target" \
            "$fix/libvirt/qemu/channel/target" \
            "$fix/srcmod/src/commands/target" "$fix/symlinked/target"; do
    case "$(verdict_for "$path")" in
        REJECT*) ok "REJECT $path — $(verdict_for "$path" | cut -d' ' -f2-)" ;;
        *)       fail "REJECT $path" "получено: $(verdict_for "$path")" ;;
    esac
done

check_eq "$(verdict_for "$fix/web/node_modules/pkg/target")" "" \
    "node_modules отсечён до классификации"

# ---------------------------------------------------------------- 2. rule A

say ""
say "2. Правило A — протухшие incremental"
sweep --dry-run --rules incremental
check_exists "$fix/live/target/debug/incremental/crate-stale" \
    "dry-run ничего не удаляет"

sweep --execute --rules incremental
check_absent "$fix/live/target/debug/incremental/crate-stale" \
    "протухший incremental удалён"
check_exists "$fix/live/target/debug/incremental/crate-fresh" \
    "свежий incremental не тронут"
check_absent "$fix/cross/target/aarch64-unknown-linux-gnu/release/incremental/crate-stale" \
    "cross-раскладка обработана"
check_exists "$fix/live/target/debug/deps/libthing.rlib" \
    "deps/ вне incremental не тронут"

sweep --execute --rules incremental
if grep -q 'would-delete=0 ' "$work/state/log/rust-target-sweep.log" \
   || tail -1 "$work/state/log/rust-target-sweep.log" | grep -q 'deleted=0'; then
    ok "повторный прогон удаляет ноль (идемпотентность)"
else
    fail "повторный прогон удаляет ноль (идемпотентность)" \
         "$(tail -1 "$work/state/log/rust-target-sweep.log")"
fi

# ---------------------------------------------------------------- 3. rule B

say ""
say "3. Правило B — спящие layout'ы"
# The guard fires on the share of dormant layouts, so it has to be exercised
# on a subtree where that share is high: across the whole fixture only 2 of 8
# layouts are dormant, which is exactly the case the guard must NOT block.
set +e
"$sweeper" --root "$fix/dormant" --execute --rules dormant \
    >"$work/out" 2>"$work/err"
guard_code=$?
set -e
check_eq "$guard_code" "3" "доля спящих выше порога — прогон прерван без --force"
check_exists "$fix/dormant/target/debug" "при сработавшем предохранителе ничего не удалено"

set +e
sweep --dry-run --rules dormant
share_code=$?
set -e
check_eq "$share_code" "0" "доля спящих ниже порога — предохранитель не мешает"
check_exists "$fix/dormant/target/debug" "dry-run правила B ничего не удаляет"

set +e
sweep --execute --force --rules dormant
b_code=$?
set -e
check_eq "$b_code" "0" "с --force прогон завершается успешно"
check_absent "$fix/dormant/target/debug"   "спящий layout debug удалён"
check_absent "$fix/dormant/target/release" "спящий layout release удалён"
check_exists "$fix/dormant/target" \
    "корень target уцелел"
check_exists "$fix/dormant/target/package/dormant-0.1.0.crate" \
    "опубликованный .crate уцелел"
check_exists "$fix/dormant/target/debian/dormant_0.1.0_amd64.deb" \
    "собранный .deb уцелел"
check_exists "$fix/dormant/target/CACHEDIR.TAG" "CACHEDIR.TAG уцелел"
check_exists "$fix/live/target/debug" "живой layout не тронут правилом B"
check_exists "$fix/live/target/doc/index.html" "target/doc уцелел"

# ---------------------------------------------------------------- 4. CLI

say ""
say "4. Контракт CLI"
set +e
sweep --dry-run --execute
usage_code=$?
set -e
check_eq "$usage_code" "2" "--dry-run вместе с --execute — ошибка аргументов"

set +e
sweep --rules nonsense
rules_code=$?
set -e
check_eq "$rules_code" "2" "неизвестное правило — ошибка аргументов"

set +e
sweep --incremental-age nope
age_code=$?
set -e
check_eq "$age_code" "2" "нечисловой возраст — ошибка аргументов"

# ---------------------------------------------------------------- 5. locking

say ""
say "5. Интерлок с идущей сборкой"
if command -v flock >/dev/null 2>&1; then
    # Rebuild: the stale entries were removed by section 2.
    build_fixture
    # Hold the lock on a descriptor of this very shell rather than in a
    # background job: `flock -c CMD &' leaves the descriptor with CMD's child,
    # so killing flock(1) would not release anything.  The sweeper runs with
    # descriptor 9 closed, so it has to take the lock the way cargo does.
    exec 9>"$fix/live/target/debug/.cargo-lock"
    flock -x 9

    "$sweeper" --root "$fix" --execute --rules incremental \
        >"$work/out" 2>"$work/err" 9>&-
    check_exists "$fix/live/target/debug/incremental/crate-stale" \
        "занятый layout не тронут"
    if grep -q 'SKIP-LOCKED' "$work/state/log/rust-target-sweep.log"; then
        ok "пропуск занятого layout'а записан в журнал"
    else
        fail "пропуск занятого layout'а записан в журнал"
    fi
    # The cross layout is a different lock and must still be swept.
    check_absent "$fix/cross/target/aarch64-unknown-linux-gnu/release/incremental/crate-stale" \
        "свободный layout обработан в том же прогоне"

    flock -u 9
    exec 9>&-

    sweep --execute --rules incremental
    check_absent "$fix/live/target/debug/incremental/crate-stale" \
        "после снятия блокировки layout обработан"
else
    say "  skip  flock(1) недоступен"
fi

# ---------------------------------------------------------------- 6. outside

say ""
say "6. Ничего за пределами фикстуры"
check_exists "$work/outside" "цель симлинка не тронута"

# ---------------------------------------------------------------- summary

say ""
if [ "$failures" -eq 0 ]; then
    say "Все проверки пройдены: $checks"
    exit 0
else
    say "Провалено $failures из $checks"
    exit 1
fi
