#!/usr/bin/env nix-shell
#!nix-shell -i bash --packages curl jq nix cacert
set -euo pipefail

script_dir="$(dirname "${BASH_SOURCE[0]}")"
sources_file="$script_dir/sources.json"

if [ -n "${NIX_SSL_CERT_FILE:-}" ] && [ -f "${NIX_SSL_CERT_FILE}" ]; then
  export SSL_CERT_FILE="${NIX_SSL_CERT_FILE}"
fi

commit_json=$(curl -sL https://api.github.com/repos/ast-grep/agent-skill/commits/main)
rev=$(jq -r '.sha' <<<"$commit_json")
version="$(jq -r '.commit.author.date | split("T")[0]' <<<"$commit_json").${rev:0:8}"
current_rev=$(jq -r '.rev' "$sources_file")

if [ "$current_rev" = "$rev" ]; then
  echo "Already at latest revision: $rev"
  exit 0
fi

echo "Updating from $current_rev to $rev"

src_hash=$(nix hash convert --hash-algo sha256 --to sri \
  "$(nix-prefetch-url --unpack "https://github.com/ast-grep/agent-skill/archive/${rev}.tar.gz" 2>/dev/null | tail -1)")

jq -n \
  --arg version "$version" \
  --arg rev "$rev" \
  --arg srcHash "$src_hash" \
  '{version: $version, rev: $rev, srcHash: $srcHash}' >"$sources_file"

echo "Done. Updated to $version ($rev)"
