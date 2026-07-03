#!/usr/bin/env nix-shell
#!nix-shell -i bash --packages coreutils curl jq nodejs nix cacert
set -euo pipefail

script_dir="$(dirname "${BASH_SOURCE[0]}")"
repo_root="$(cd "$script_dir/../../.." && pwd -P)"
sources_file="$script_dir/sources.json"
lockfile="$script_dir/package-lock.json"

if [ -n "${NIX_SSL_CERT_FILE:-}" ] && [ -f "${NIX_SSL_CERT_FILE}" ]; then
  export SSL_CERT_FILE="${NIX_SSL_CERT_FILE}"
  export NPM_CONFIG_CAFILE="${NIX_SSL_CERT_FILE}"
fi

current_version=$(jq -r '.version' "$sources_file")

# Latest published version from the npm registry
version=$(curl -sL "https://registry.npmjs.org/@monotykamary/localterm/latest" | jq -r '.version')

if [ "$current_version" = "$version" ]; then
  echo "Already at latest version: $version, ensuring npm deps hash is up to date"
else
  echo "Updating from $current_version to $version"
fi

# Prefetch the published tarball and compute its hash
src_hash=$(nix hash convert --hash-algo sha256 --to sri \
  "$(nix-prefetch-url \
    "https://registry.npmjs.org/@monotykamary/localterm/-/localterm-${version}.tgz" \
    2>/dev/null | tail -1)")

# Extract the tarball and regenerate package-lock.json from its package.json
tmpdir=$(mktemp -d)
trap 'rm -rf "$tmpdir"' EXIT

curl -sL "https://registry.npmjs.org/@monotykamary/localterm/-/localterm-${version}.tgz" |
  tar xz -C "$tmpdir" --strip-components=1

(cd "$tmpdir" && npm install --package-lock-only --ignore-scripts)
cp "$tmpdir/package-lock.json" "$lockfile"

# Write sources.json with a fake npmDepsHash, then build to get the real one.
# Use the flake (path:.) rather than <nixpkgs>: the native node-pty module
# needs the flake's Darwin SDK setup to compile from source.
jq -n \
  --arg version "$version" \
  --arg srcHash "$src_hash" \
  --arg npmDepsHash "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=" \
  '{version: $version, srcHash: $srcHash, npmDepsHash: $npmDepsHash}' >"$sources_file"

build_log=$(nix build --no-link --print-out-paths "path:${repo_root}#localterm.npmDeps" 2>&1 || true)
npm_deps_hash=$(echo "$build_log" | grep 'got:' | head -1 | sed 's/.*got: *//')

if [ -z "$npm_deps_hash" ]; then
  echo "ERROR: Failed to determine npmDeps hash"
  nix build --no-link "path:${repo_root}#localterm.npmDeps" 2>&1 | tail -10
  exit 1
fi

# Write final sources.json with the real npmDepsHash
jq -n \
  --arg version "$version" \
  --arg srcHash "$src_hash" \
  --arg npmDepsHash "$npm_deps_hash" \
  '{version: $version, srcHash: $srcHash, npmDepsHash: $npmDepsHash}' >"$sources_file"

echo "Done. Updated to $version"
