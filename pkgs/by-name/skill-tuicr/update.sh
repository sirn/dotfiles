#!/usr/bin/env nix-shell
#!nix-shell -i bash --packages curl jq nix cacert
set -euo pipefail

script_dir="$(dirname "${BASH_SOURCE[0]}")"
sources_file="$script_dir/sources.json"

if [ -n "${NIX_SSL_CERT_FILE:-}" ] && [ -f "${NIX_SSL_CERT_FILE:-}" ]; then
  export SSL_CERT_FILE="${NIX_SSL_CERT_FILE}"
fi

latest=$(curl -sL https://api.github.com/repos/agavra/tuicr/releases/latest)
tag=$(jq -r '.tag_name' <<<"$latest")
version="${tag#v}"

current_version=$(jq -r '.version' "$sources_file")
if [ "$current_version" = "$version" ]; then
  echo "Already at latest version: $version"
  exit 0
fi

# Resolve the commit SHA the tag points at (handles both lightweight and annotated tags)
ref=$(curl -sL "https://api.github.com/repos/agavra/tuicr/git/refs/tags/${tag}")
ref_type=$(jq -r '.object.type' <<<"$ref")
if [ "$ref_type" = "tag" ]; then
  rev=$(curl -sL "https://api.github.com/repos/agavra/tuicr/git/tags/$(jq -r '.object.sha' <<<"$ref")" | jq -r '.object.sha')
else
  rev=$(jq -r '.object.sha' <<<"$ref")
fi

echo "Updating to ${version} (${rev})"

src_hash=$(nix hash convert --hash-algo sha256 --to sri \
  "$(nix-prefetch-url --unpack "https://github.com/agavra/tuicr/archive/${rev}.tar.gz" 2>/dev/null | tail -1)")

jq -n \
  --arg version "$version" \
  --arg rev "$rev" \
  --arg srcHash "$src_hash" \
  '{version: $version, rev: $rev, srcHash: $srcHash}' >"$sources_file"

echo "Done. Updated to $version ($rev)"
