#!/usr/bin/env nix-shell
#!nix-shell -i bash --packages curl jq nix cacert
set -euo pipefail

script_dir="$(dirname "${BASH_SOURCE[0]}")"
sources_file="$script_dir/sources.json"

if [ -n "${NIX_SSL_CERT_FILE:-}" ] && [ -f "${NIX_SSL_CERT_FILE:-}" ]; then
  export SSL_CERT_FILE="${NIX_SSL_CERT_FILE}"
fi

commit=$(curl -sL https://api.github.com/repos/hashicorp/agent-skills/commits/main)
rev=$(jq -r '.sha' <<<"$commit")
commit_date=$(jq -r '.commit.committer.date[0:10]' <<<"$commit")
version="0-unstable-${commit_date}"

current_rev=$(jq -r '.rev' "$sources_file")
if [ "$current_rev" = "$rev" ]; then
  echo "Already at latest revision: $version ($rev)"
  exit 0
fi

echo "Updating to ${version} (${rev})"

src_hash=$(nix hash convert --hash-algo sha256 --to sri \
  "$(nix-prefetch-url --unpack "https://github.com/hashicorp/agent-skills/archive/${rev}.tar.gz" 2>/dev/null | tail -1)")

jq -n \
  --arg version "$version" \
  --arg rev "$rev" \
  --arg srcHash "$src_hash" \
  '{version: $version, rev: $rev, srcHash: $srcHash}' >"$sources_file"

echo "Done. Updated to $version ($rev)"
