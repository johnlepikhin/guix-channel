#!/bin/sh
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

# rb-wrap ID MODE COMMAND [ARG]... -- run COMMAND for rb inside the container.
#
# The command's process group is recorded in $HOME/.rb/run/ID, which is
# what `rb ps' and `rb kill' read.
#
# MODE "tty": the command takes over the terminal; Ctrl-C reaches it
# through the pty.
#
# MODE "watch": no terminal.  docker exec does not kill a command whose
# client went away, so rb writes a newline to our stdin every few seconds
# instead, and the command, in a session of its own with stdin from
# /dev/null, is killed once that heartbeat stops: on EOF when rb or ssh
# exits, or after a silence when the connection drops without closing.

id=$1
mode=$2
shift 2

run="$HOME/.rb/run"
mkdir -p "$run"

if [ "$mode" = tty ]; then
    echo "$$ $(date +%s) $*" > "$run/$id"
    exec "$@"
fi

# Asynchronous lists of a non-interactive shell read /dev/null, so the
# heartbeat is kept on descriptor 3 for the watcher.
exec 3<&0

# A background job of a non-interactive shell is no group leader, so
# setsid(1) calls setsid() without forking: the PID is the process group.
setsid "$@" < /dev/null 3<&- &
pgid=$!
echo "$pgid $(date +%s) $*" > "$run/$id"

(
    while timeout 30 sh -c 'IFS= read -r _' <&3; do :; done
    # The group, or the process alone if it has not called setsid() yet.
    kill -TERM -- "-$pgid" 2> /dev/null || kill -TERM "$pgid" 2> /dev/null \
        || exit 0
    sleep 10
    kill -KILL -- "-$pgid" 2> /dev/null || kill -KILL "$pgid" 2> /dev/null
) &
watcher=$!
exec 3<&-

wait "$pgid"
status=$?
kill "$watcher" 2> /dev/null
rm -f "$run/$id"
exit "$status"
