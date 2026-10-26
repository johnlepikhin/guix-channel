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

"""Copy a configuration file with its secrets replaced by a placeholder.

Usage: config-history-redact.py KEYS SOURCE TARGET

KEYS is a comma-separated list of key names.  A key is secret when its
name, lowercased, equals one of them or ends with "_" and one of them, so
"token" also covers "push_token".  JSON files are parsed and written back
indented; any other text is treated as YAML, line by line, which covers
block mappings and sequences as Home Assistant and Zigbee2MQTT write them.
Files that are not UTF-8 are refused rather than copied unredacted.
"""

import json
import re
import sys

PLACEHOLDER = "<redacted>"


def is_secret(key, keys):
    key = key.lower()
    return any(key == k or key.endswith("_" + k) for k in keys)


def redact_json(value, keys):
    if isinstance(value, dict):
        return {k: (PLACEHOLDER if is_secret(k, keys) and v not in (None, "")
                    else redact_json(v, keys))
                for k, v in value.items()}
    if isinstance(value, list):
        return [redact_json(v, keys) for v in value]
    return value


YAML_KEY = re.compile(r"^(\s*)(?:-\s+)?([\"']?)([^\"':#\s]+)\2:(\s*)(.*)$")


def indent_of(line):
    return len(line) - len(line.lstrip(" "))


def redact_yaml(text, keys):
    lines = text.splitlines(keepends=True)
    out = []
    i = 0
    while i < len(lines):
        line = lines[i]
        match = YAML_KEY.match(line)
        i += 1
        if not (match and is_secret(match.group(3), keys)):
            out.append(line)
            continue
        # Column of the key itself, also after a "- " sequence marker.
        indent = match.start(3) - len(match.group(2))
        block = not match.group(5).strip()
        prefix = line[:match.start(5)].rstrip()
        out.append(f"{prefix} {PLACEHOLDER}\n")
        # The value continues on deeper lines; a block value may also be a
        # sequence at the same indentation.
        while i < len(lines):
            nxt = lines[i]
            if not nxt.strip():
                break
            deeper = indent_of(nxt) > indent
            sibling_item = (block and indent_of(nxt) == indent
                            and nxt.lstrip(" ").startswith("- "))
            if not (deeper or sibling_item):
                break
            i += 1
    return "".join(out)


def main():
    keys = [k.strip().lower() for k in sys.argv[1].split(",") if k.strip()]
    source, target = sys.argv[2], sys.argv[3]
    with open(source, encoding="utf-8") as port:
        text = port.read()
    try:
        data = json.loads(text)
    except ValueError:
        result = redact_yaml(text, keys)
    else:
        result = json.dumps(redact_json(data, keys), indent=2,
                            ensure_ascii=False) + "\n"
    with open(target, "w", encoding="utf-8") as port:
        port.write(result)


if __name__ == "__main__":
    main()
