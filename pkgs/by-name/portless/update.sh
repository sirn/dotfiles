#!/usr/bin/env nix-shell
#!nix-shell -i bash --packages coreutils curl jq nix cacert
set -euo pipefail

script_dir="$(dirname "${BASH_SOURCE[0]}")"
repo_root="$(cd "$script_dir/../../.." && pwd -P)"
sources_file="$script_dir/sources.json"

if [ -n "${NIX_SSL_CERT_FILE:-}" ] && [ -f "${NIX_SSL_CERT_FILE}" ]; then
  export SSL_CERT_FILE="${NIX_SSL_CERT_FILE}"
fi

current_version=$(jq -r '.version' "$sources_file")

# Latest published version from the npm registry
version=$(curl -sL "https://registry.npmjs.org/portless/latest" | jq -r '.version')

if [ "$current_version" = "$version" ]; then
  echo "Already at latest version: $version"
  exit 0
fi

echo "Updating from $current_version to $version"

# Prefetch the published tarball and compute its hash
src_hash=$(nix hash convert --hash-algo sha256 --to sri \
  "$(nix-prefetch-url \
    "https://registry.npmjs.org/portless/-/portless-${version}.tgz" \
    2>/dev/null | tail -1)")

jq -n \
  --arg version "$version" \
  --arg srcHash "$src_hash" \
  '{version: $version, srcHash: $srcHash}' >"$sources_file"

echo "Done. Updated to $version"
