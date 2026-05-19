#!/usr/bin/env nix-shell
#!nix-shell -i bash --packages coreutils curl jq nix cacert
set -euo pipefail

script_dir="$(dirname "${BASH_SOURCE[0]}")"
sources_file="$script_dir/sources.json"

if [ -n "${NIX_SSL_CERT_FILE:-}" ] && [ -f "${NIX_SSL_CERT_FILE}" ]; then
  export SSL_CERT_FILE="${NIX_SSL_CERT_FILE}"
  export NPM_CONFIG_CAFILE="${NIX_SSL_CERT_FILE}"
fi

current_version=$(jq -r '.version' "$sources_file")
version=$(curl -s https://api.github.com/repos/earendil-works/pi/tags |
  jq -r '.[0].name' | sed 's/^v//')

if [ "$current_version" = "$version" ]; then
  echo "Already at latest version: $version"
  exit 0
fi

echo "Updating from $current_version to $version"

src_hash=$(nix hash convert --hash-algo sha256 --to sri \
  "$(nix-prefetch-url --unpack \
    "https://github.com/earendil-works/pi/archive/refs/tags/v${version}.tar.gz" \
    2>/dev/null | tail -1)")

jq -n \
  --arg version "$version" \
  --arg srcHash "$src_hash" \
  --arg npmDepsHash "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=" \
  '{version: $version, srcHash: $srcHash, npmDepsHash: $npmDepsHash}' \
  >"$sources_file"

build_log=$(nix-build -E \
  'let pkgs = import <nixpkgs> {}; in pkgs.callPackage ./pkgs/by-name/pi-coding-agent/package.nix {}' \
  2>&1 || true)
npm_deps_hash=$(echo "$build_log" | grep 'got:' | head -1 | sed 's/.*got: *//')

if [ -z "$npm_deps_hash" ]; then
  echo "ERROR: Failed to determine npmDepsHash. Build output:"
  echo "$build_log" | tail -20
  exit 1
fi

jq -n \
  --arg version "$version" \
  --arg srcHash "$src_hash" \
  --arg npmDepsHash "$npm_deps_hash" \
  '{version: $version, srcHash: $srcHash, npmDepsHash: $npmDepsHash}' \
  >"$sources_file"

echo "Verifying build..."
nix-build -E \
  'let pkgs = import <nixpkgs> {}; in pkgs.callPackage ./pkgs/by-name/pi-coding-agent/package.nix {}' \
  --no-out-link

echo "Done. Updated to $version"
