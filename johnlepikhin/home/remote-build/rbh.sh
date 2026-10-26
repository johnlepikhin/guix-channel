#!/usr/bin/env bash
# GNU Guix --- Functional package management for GNU
# Copyright © 2026 Evgenii Lepikhin <johnlepikhin@gmail.com>
#
# This file is not part of GNU Guix.
#
# GNU Guix is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
#
# GNU Guix is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

# Server side of rb, run over ssh:
#
#   RB_HOME=DIR bash rbh ROOT PROJECT SPEC WRAP COMMAND [ARG]...
#
# ROOT is the directory of all projects, relative to $HOME or absolute;
# SPEC the project's specification (bash assignments written by the Guix
# service); WRAP the rb-wrap script; RB_HOME the $HOME of the container,
# which is the laptop user's home so that paths match.
#
# Errors of rb itself exit with 75 (EX_TEMPFAIL) and an "rb:" prefix, so
# that they are not mistaken for a failure of the user's command.

set -euo pipefail

readonly EX_TEMPFAIL=75
die() { printf 'rb: %s\n' "$*" >&2; exit "$EX_TEMPFAIL"; }
note() { printf 'rb: %s\n' "$*" >&2; }

cd "$HOME"
[ $# -ge 5 ] || die "usage: rbh ROOT PROJECT SPEC WRAP COMMAND [ARG]..."
root=$1 project=$2 spec=$3 wrap=$4 command=$5
shift 5
: "${RB_HOME:?RB_HOME is not set}"

mkdir -p "$root"
root=$(cd "$root" && pwd)
dir="$root/$project"
src="$dir/src"
home="$dir/home"
log="$dir/rb-setup.log"
run="$home/.rb/run"
container="rb-$project"
mkdir -p "$src" "$run" "$root/tmp"
chmod 1777 "$root/tmp"

# shellcheck source=/dev/null
. "$spec"

label() {
    docker inspect -f '{{index .Config.Labels "rb.hash"}}' "$1" 2> /dev/null || true
}
running() {
    [ "$(docker inspect -f '{{.State.Running}}' "$1" 2> /dev/null)" = true ]
}
exists() {
    docker inspect "$1" > /dev/null 2>&1
}
digest() {
    sha256sum | cut -c1-32
}

# Print the IDs of commands still running in the container, forgetting the
# others.
live_commands() {
    local file pgid
    for file in "$run"/*; do
        [ -e "$file" ] || continue
        read -r pgid _ < "$file" || continue
        if running "$container" \
                && docker exec "$container" kill -0 -- "-$pgid" 2> /dev/null; then
            printf '%s\n' "${file##*/}"
        else
            rm -f "$file"
        fi
    done
}

# Set IMAGE_REF and IMAGE_ID, building or pulling the image if needed.
ensure_image() {
    if [ -n "$DOCKERFILE" ]; then
        local file="$src/$DOCKERFILE" context arg
        local -a args=()
        [ -f "$file" ] || die "$DOCKERFILE is missing from the synced tree"
        IMAGE_REF="rb/$project:$({ sha256sum < "$file"
                                   printf '%s\n' "${BUILD_ARGS[@]}" "$SCHEMA"
                                 } | digest | cut -c1-16)"
        if ! docker image inspect "$IMAGE_REF" > /dev/null 2>&1; then
            if [ -n "$CONTEXT" ]; then
                context="$src/$CONTEXT"
            else
                # The Dockerfile copies nothing in: build without sending
                # the tree, whose target/ may hold hundreds of gigabytes.
                context="$dir/.rb-empty-context"
                mkdir -p "$context"
            fi
            for arg in "${BUILD_ARGS[@]}"; do args+=(--build-arg "$arg"); done
            note "building image $IMAGE_REF, log in $log"
            docker build -t "$IMAGE_REF" -f "$file" "${args[@]}" "$context" \
                   >> "$log" 2>&1 \
                || die "image build failed, see $log"
        fi
    else
        IMAGE_REF=$IMAGE
        if ! docker image inspect "$IMAGE_REF" > /dev/null 2>&1; then
            note "pulling $IMAGE_REF"
            docker pull -q "$IMAGE_REF" >> "$log" 2>&1 \
                || die "pull of $IMAGE_REF failed, see $log"
        fi
    fi
    IMAGE_ID=$(docker image inspect -f '{{.Id}}' "$IMAGE_REF")
}

ensure_services() {
    local i name
    for i in "${!SERVICES[@]}"; do
        declare -n svc_image="SVC_${i}_IMAGE" svc_hash="SVC_${i}_HASH" \
                svc_args="SVC_${i}_ARGS" svc_cmd="SVC_${i}_CMD"
        name="$container-${SERVICES[$i]}"
        if [ "$(label "$name")" != "$svc_hash" ]; then
            docker rm -f "$name" > /dev/null 2>&1 || true
            note "starting service ${SERVICES[$i]}"
            docker run -d --name "$name" --label "rb.hash=$svc_hash" \
                   --restart unless-stopped "${svc_args[@]}" "$svc_image" "${svc_cmd[@]}" \
                   >> "$log" 2>&1 \
                || die "service ${SERVICES[$i]} failed to start, see $log"
        elif ! running "$name"; then
            docker start "$name" > /dev/null \
                || die "service ${SERVICES[$i]} failed to start"
        fi
        unset -n svc_image svc_hash svc_args svc_cmd
    done
}

create_container() {
    local secret variable
    local -a args=(-d --init --name "$container" --label "rb.hash=$1"
                   --network host --user "$(id -u):$(id -g)"
                   --entrypoint sleep
                   -e "HOME=$RB_HOME" -w "$SRC_PATH"
                   -v "$home:$RB_HOME" -v "$src:$SRC_PATH" -v "$root/tmp:/tmp")
    for secret in "${SECRET_DIRECTORIES[@]}"; do
        [ -d "$secret" ] || die "secret directory $secret is missing on the server"
        args+=(-v "$secret:$secret:ro")
    done
    for variable in "${ENVIRONMENT[@]}"; do
        args+=(-e "${variable//\$HOME/$RB_HOME}")
    done
    args+=("${DOCKER_OPTIONS[@]}")
    note "creating container $container"
    docker run "${args[@]}" "$IMAGE_REF" infinity >> "$log" 2>&1 \
        || die "container failed to start, see $log"
}

# Make the uid of the server user a passwd entry whose home is RB_HOME: Java
# takes user.home from there, not from $HOME.  Images may already use the
# uid (ubuntu, node), so the entry's home is rewritten rather than added.
passwd_script='
u=$1 g=$2 h=$3
if grep -q "^[^:]*:[^:]*:$u:" /etc/passwd; then
    sed -i "s#^\([^:]*:[^:]*:$u:[^:]*:[^:]*:\)[^:]*#\1$h#" /etc/passwd
else
    echo "rb:x:$u:$g::$h:/bin/sh" >> /etc/passwd
fi'

setup_container() {
    local marker="$home/.rb-setup-$1"
    [ -e "$marker" ] && return 0
    note "setting up $container, log in $log"
    if { docker exec -u 0 "$container" sh -c "$passwd_script" sh \
                "$(id -u)" "$(id -g)" "$RB_HOME" \
            && { [ -z "$ROOT_SETUP" ] \
                     || docker exec -u 0 "$container" sh -c "$ROOT_SETUP"; } \
            && { [ -z "$SETUP" ] \
                     || docker exec "$container" sh -c "$SETUP"; }
       } >> "$log" 2>&1; then
        rm -f "$home"/.rb-setup-*
        touch "$marker"
    else
        # Next time starts from a fresh container, since the setup may have
        # left it half-done.
        docker rm -f "$container" > /dev/null 2>&1 || true
        die "setup of $container failed, see $log"
    fi
}

# ensure [force]: start the services and an up-to-date container.
ensure() {
    local force=${1:-} want busy
    ensure_services
    ensure_image
    want=$(printf '%s\n' "$CONFIG_HASH" "$IMAGE_ID" "$RB_HOME" \
                  "$(id -u):$(id -g)" | digest)
    if [ "$force" = force ] || [ "$(label "$container")" != "$want" ]; then
        if exists "$container"; then
            busy=$(live_commands | wc -l)
            if [ "$busy" -gt 0 ] && [ "$force" != force ]; then
                die "container busy ($busy commands running); run 'rb restart' when idle"
            fi
            docker rm -f "$container" > /dev/null
        fi
        create_container "$want"
    elif ! running "$container"; then
        docker start "$container" > /dev/null || die "container failed to start"
    fi
    mkdir -p "$home/.rb"
    cmp -s "$wrap" "$home/.rb/rb-wrap" \
        || install -m 755 "$wrap" "$home/.rb/rb-wrap"
    setup_container "$want"
}

# Delete the NUL-separated paths on stdin from the tree, refusing any that
# could reach outside it.
delete_paths() {
    local path parent current component
    local -a components
    while IFS= read -r -d '' path; do
        case "/$path/" in
            //|*/../*|*/./*) note "refusing to delete '$path'"; continue ;;
        esac
        case "$path" in
            /*) note "refusing to delete '$path'"; continue ;;
        esac
        parent=${path%/*}
        [ "$parent" = "$path" ] && parent=
        current=$src
        IFS=/ read -ra components <<< "$parent"
        for component in "${components[@]}"; do
            current="$current/$component"
            if [ -L "$current" ]; then
                note "refusing to delete '$path': $current is a symlink"
                continue 2
            fi
        done
        rm -f -- "$src/$path"
        while [ -n "$parent" ] && rmdir -- "$src/$parent" 2> /dev/null; do
            case "$parent" in
                */*) parent=${parent%/*} ;;
                *) parent= ;;
            esac
        done
    done
}

# fetch KIND PATTERN...: tar of the files matching the patterns, KIND src
# (relative to the tree) or tmp (relative to the shared /tmp).
fetch() {
    local kind=$1 base pattern literal start
    shift
    case "$kind" in
        src) base=$src ;;
        tmp) base="$root/tmp" ;;
        *) die "unknown fetch root $kind" ;;
    esac
    for pattern in "$@"; do
        case "/$pattern/" in */../*) die "refusing pattern '$pattern'" ;; esac
        # Search only below the literal part of the pattern: a pattern for
        # target/debian must not walk all of target.
        literal=${pattern%%[*?[]*}
        if [ "$literal" = "$pattern" ]; then
            start="./$pattern"
        else
            case "$literal" in
                */*) start="./${literal%/*}" ;;
                *) start=. ;;
            esac
        fi
        [ -e "$base/$start" ] || continue
        (cd "$base" && find "$start" -path "./$pattern" -type f -print0)
    done | (cd "$base" && tar -cf - --null -T -)
}

status() {
    printf 'project:    %s\n' "$project"
    if exists "$container"; then
        printf 'container:  %s (%s, image %s, started %s)\n' "$container" \
               "$(docker inspect -f '{{.State.Status}}' "$container")" \
               "$(docker inspect -f '{{.Config.Image}}' "$container")" \
               "$(docker inspect -f '{{.State.StartedAt}}' "$container")"
        if ls "$home"/.rb-setup-"$(label "$container")" > /dev/null 2>&1; then
            printf 'setup:      done\n'
        else
            printf 'setup:      pending or failed, see %s\n' "$log"
        fi
    else
        printf 'container:  none (created on the next command)\n'
    fi
    for i in "${!SERVICES[@]}"; do
        local name="$container-${SERVICES[$i]}"
        printf 'service:    %s (%s)\n' "${SERVICES[$i]}" \
               "$(docker inspect -f '{{.State.Status}}' "$name" 2> /dev/null \
                     | grep . || echo none)"
    done
    printf 'commands:   %s running\n' "$(live_commands | wc -l)"
    printf 'disk:       %s\n' "$(du -sh "$src" "$home" 2> /dev/null \
                                   | awk '{printf "%s %s  ", $1, $2}')"
}

ps_commands() {
    local id pgid started rest
    for id in $(live_commands); do
        read -r pgid started rest < "$run/$id"
        printf '%s  pgid %s  since %s  %s\n' "$id" "$pgid" \
               "$(date -d "@$started" '+%F %T')" "$rest"
    done
}

kill_commands() {
    local id pgid
    for id in $(live_commands); do
        if [ $# -eq 0 ] || [ "$1" = "$id" ]; then
            read -r pgid _ < "$run/$id"
            docker exec "$container" kill -TERM -- "-$pgid" 2> /dev/null || true
            note "sent TERM to $id"
        fi
    done
}

down() {
    local i
    for i in "${!SERVICES[@]}"; do
        docker rm -f "$container-${SERVICES[$i]}" > /dev/null 2>&1 || true
    done
    docker rm -f "$container" > /dev/null 2>&1 || true
    docker images -q "rb/$project" | sort -u | xargs -r docker rmi -f > /dev/null
    rm -f "$home"/.rb-setup-*
    if [ "${1:-}" = purge ]; then
        rm -rf -- "$dir"
        note "removed $dir"
    fi
}

case "$command" in
    # Hold the project lock while stdin stays open.
    lock)
        exec 9> "$dir/.rb-lock"
        if ! flock -n 9; then
            note "waiting for another rb of $project"
            flock 9
        fi
        echo locked
        exec cat > /dev/null ;;
    # Server clock, git fingerprint and manifest of the last sync.
    state)
        date +%s.%N
        cat "$dir/.rb-git-fingerprint" 2> /dev/null || true
        echo
        cat "$dir/.rb-manifest" 2> /dev/null || true ;;
    # The tree as git on the server sees it, for `rb sync --full'.
    list)
        git -C "$src" ls-files -z --cached --others --exclude-standard \
            2> /dev/null || true ;;
    delete) delete_paths ;;
    commit)
        printf '%s' "$1" > "$dir/.rb-git-fingerprint.tmp"
        mv "$dir/.rb-git-fingerprint.tmp" "$dir/.rb-git-fingerprint"
        cat > "$dir/.rb-manifest.tmp"
        mv "$dir/.rb-manifest.tmp" "$dir/.rb-manifest" ;;
    ensure) ensure "${1:-}" ;;
    exec)
        mode=$1 workdir=$2 id=$3
        shift 3
        flags=(-i)
        [ "$mode" = tty ] && flags=(-it)
        exec docker exec "${flags[@]}" -w "$workdir" "$container" \
             "$RB_HOME/.rb/rb-wrap" "$id" "$mode" "$@" ;;
    fetch) fetch "$@" ;;
    status) status ;;
    ps) ps_commands ;;
    kill) kill_commands "$@" ;;
    pull)
        [ -n "$IMAGE" ] && docker pull "$IMAGE"
        for i in "${!SERVICES[@]}"; do
            declare -n image="SVC_${i}_IMAGE"
            docker pull "$image"
            unset -n image
        done ;;
    down) down "$@" ;;
    *) die "unknown command $command" ;;
esac
