#!/usr/bin/env python3
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

"""Report packages of this channel whose upstream has a newer version.

Inventory is taken from the channel itself (via `guix repl'), so the script
never goes stale when packages are added or removed.  Upstream versions come
from the registry implied by each package's source URI: npm, crates.io,
GitHub, GitLab, PyPI, MetaCPAN, RubyGems.  Everything else is reported as
`manual' -- no guessing.

Usage:
    scripts/check-updates.py [--channel DIR] [--json] [--all] [PACKAGE...]

Environment:
    GITHUB_TOKEN  used for GitHub API calls; without it the anonymous rate
                  limit (60 requests/hour) is easily exhausted.
"""

import argparse
import json
import os
import re
import subprocess
import sys
import tempfile
import urllib.error
import urllib.parse
import urllib.request
from concurrent.futures import ThreadPoolExecutor

# Packages that are deliberately pinned: bootstrap toolchains and dated
# nightly snapshots, where "newest upstream" is not what we want.
PINNED = re.compile(r"^(rust|rust-next|rust-nightly|rust-src|rust-clippy|"
                    r"rust-binary(-complete)?|google-chromium)$")

INVENTORY_SCM = r"""
(use-modules (guix discovery) (guix packages) (guix git-download)
             (srfi srfi-1))

(define mods
  (all-modules (list (cons "%CHANNEL%" "johnlepikhin"))))

(define (uri-of source)
  (if (origin? source)
      (let ((uri (origin-uri source)))
        (cond ((string? uri) uri)
              ((git-reference? uri)
               (string-append (git-reference-url uri)
                              "@" (git-reference-commit uri)))
              ((and (pair? uri) (string? (car uri))) (car uri))
              (else "")))
      ""))

(for-each
 (lambda (package)
   (format #t "~a\t~a\t~a\n"
           (package-name package) (package-version package)
           (uri-of (package-source package))))
 (sort (delete-duplicates
        (fold-module-public-variables
         (lambda (obj acc) (if (package? obj) (cons obj acc) acc))
         '() mods)
        eq?)
       (lambda (a b) (string< (package-name a) (package-name b)))))
"""


def inventory(channel):
    """Return [(name, version, uri)] for every package defined in CHANNEL."""
    with tempfile.NamedTemporaryFile("w", suffix=".scm", delete=False) as f:
        f.write(INVENTORY_SCM.replace("%CHANNEL%", channel))
        script = f.name
    try:
        out = subprocess.run(["guix", "repl", "-L", channel, script],
                             capture_output=True, text=True, check=True).stdout
    finally:
        os.unlink(script)

    packages = []
    for line in out.splitlines():
        fields = line.split("\t")
        if len(fields) == 3:
            packages.append(tuple(fields))
    return packages


def fetch_json(url, headers=None):
    request = urllib.request.Request(url, headers=headers or {})
    request.add_header("User-Agent", "guix-channel-update-check")
    with urllib.request.urlopen(request, timeout=30) as response:
        return json.load(response)


def github_headers():
    token = os.environ.get("GITHUB_TOKEN")
    return {"Authorization": f"Bearer {token}"} if token else {}


def strip_prefix(tag):
    """Extract the version out of a release tag.

    Handles `v1.2.3', `release-1.2.3' and project-prefixed tags such as
    `intel-gmmlib-22.10.0'.  Tags with no dotted version (`WW44.4_25_ptl_pv')
    are returned verbatim rather than mangled into a fake version."""
    match = re.search(r"(?:^|[-_/])v?(\d+(?:\.\d+)+[A-Za-z0-9.+_-]*)$", tag)
    return match.group(1) if match else tag


def version_key(version):
    """Sort key comparing numeric components, with a text tail-breaker."""
    return ([int(n) for n in re.findall(r"\d+", version)], version)


def latest_github(owner, repo):
    headers = github_headers()
    api = f"https://api.github.com/repos/{owner}/{repo}"
    try:
        return strip_prefix(fetch_json(f"{api}/releases/latest",
                                       headers)["tag_name"])
    except urllib.error.HTTPError as error:
        if error.code not in (403, 404):
            raise
    # No published release (or only pre-releases): fall back to tags.
    tags = [strip_prefix(t["name"])
            for t in fetch_json(f"{api}/tags?per_page=100", headers)]
    tags = [t for t in tags if re.search(r"\d", t)]
    return max(tags, key=version_key) if tags else None


def latest_gitlab(host, project):
    project = urllib.parse.quote(project, safe="")
    tags = fetch_json(f"https://{host}/api/v4/projects/{project}"
                      f"/repository/tags?per_page=100")
    names = [strip_prefix(t["name"]) for t in tags if re.search(r"\d", t["name"])]
    return max(names, key=version_key) if names else None


def latest_upstream(uri):
    """Return (latest-version, source-label) for URI, or (None, label)."""
    npm = re.search(r"registry\.npmjs\.org/((?:@[^/]+/)?[^/]+)/-/", uri)
    if npm:
        name = urllib.parse.quote(npm.group(1), safe="@")
        data = fetch_json(f"https://registry.npmjs.org/{name}")
        return data["dist-tags"].get("latest"), "npm"

    crate = re.search(r"crates\.io/api/v1/crates/([^/]+)/", uri)
    if crate:
        data = fetch_json(f"https://crates.io/api/v1/crates/{crate.group(1)}")
        return (data["crate"].get("max_stable_version")
                or data["crate"]["max_version"]), "crates.io"

    pypi = re.search(r"files\.pythonhosted\.org/packages/source/./([^/]+)/", uri)
    if pypi:
        data = fetch_json(f"https://pypi.org/pypi/{pypi.group(1)}/json")
        return data["info"]["version"], "pypi"

    cpan = re.search(r"mirror://cpan/authors/id/[^/]+/[^/]+/[^/]+/"
                     r"(.+?)-v?[\d._]+\.(?:tar\.gz|tgz|zip)$", uri)
    if cpan:
        data = fetch_json("https://fastapi.metacpan.org/v1/release/"
                          + cpan.group(1))
        return data["version"], "metacpan"

    gem = re.search(r"rubygems\.org/downloads/(.+)-[\d.]+\.gem$", uri)
    if gem:
        data = fetch_json(f"https://rubygems.org/api/v1/gems/{gem.group(1)}.json")
        return data["version"], "rubygems"

    github = re.search(r"github\.com/([^/]+)/([^/@]+?)(?:\.git)?[/@]", uri + "@")
    if github:
        return latest_github(github.group(1), github.group(2)), "github"

    gitlab = re.search(r"(gitlab\.[^/]+)/(.+?)(?:\.git)?@", uri)
    if gitlab:
        return latest_gitlab(gitlab.group(1), gitlab.group(2)), "gitlab"

    return None, "manual"


def check(entry):
    name, version, uri = entry
    if PINNED.match(name):
        return {"name": name, "current": version, "latest": None,
                "source": "pinned", "status": "pinned"}
    try:
        latest, source = latest_upstream(uri)
    except Exception as error:                          # network, API, parsing
        return {"name": name, "current": version, "latest": None,
                "source": "error", "status": f"error: {error}"}

    if latest is None:
        status = "unknown"
    elif latest == version:
        status = "current"
    elif version_key(latest) > version_key(version):
        status = "outdated"
    else:
        status = "ahead"                                # local newer than upstream
    return {"name": name, "current": version, "latest": latest,
            "source": source, "status": status}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("packages", nargs="*",
                        help="only check these packages (default: all)")
    parser.add_argument("--channel", default=os.path.dirname(
        os.path.dirname(os.path.abspath(__file__))),
        help="channel checkout to inspect")
    parser.add_argument("--json", action="store_true",
                        help="emit machine-readable output")
    parser.add_argument("--all", action="store_true",
                        help="also list up-to-date and unknown packages")
    parser.add_argument("--jobs", type=int, default=8,
                        help="parallel upstream queries")
    args = parser.parse_args()

    packages = inventory(args.channel)
    if args.packages:
        wanted = set(args.packages)
        packages = [p for p in packages if p[0] in wanted]

    with ThreadPoolExecutor(max_workers=args.jobs) as pool:
        results = list(pool.map(check, packages))

    if args.json:
        json.dump(results, sys.stdout, indent=2)
        print()
        return 0

    shown = results if args.all else [
        r for r in results if r["status"] not in ("current", "pinned")]
    order = {"outdated": 0, "ahead": 1, "unknown": 2, "current": 3, "pinned": 4}
    shown.sort(key=lambda r: (order.get(r["status"].split(":")[0], 5), r["name"]))

    width = max((len(r["name"]) for r in shown), default=4)
    for result in shown:
        print(f"{result['name']:<{width}}  {result['current']:>16} -> "
              f"{result['latest'] or '?':<16} "
              f"[{result['source']}] {result['status']}")

    outdated = sum(1 for r in results if r["status"] == "outdated")
    print(f"\n{outdated} outdated of {len(results)} packages", file=sys.stderr)
    return 1 if outdated else 0


if __name__ == "__main__":
    sys.exit(main())
