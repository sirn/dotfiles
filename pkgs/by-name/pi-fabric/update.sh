#!/usr/bin/env nix-shell
#!nix-shell -i bash --packages coreutils curl jq nodejs nix cacert
set -euo pipefail

script_dir="$(dirname "${BASH_SOURCE[0]}")"
sources_file="$script_dir/sources.json"
lockfile="$script_dir/package-lock.json"
pkgjson="$script_dir/package.json"

if [ -n "${NIX_SSL_CERT_FILE:-}" ] && [ -f "${NIX_SSL_CERT_FILE}" ]; then
  export SSL_CERT_FILE="${NIX_SSL_CERT_FILE}"
  export NPM_CONFIG_CAFILE="${NIX_SSL_CERT_FILE}"
fi

export NPM_CONFIG_CACHE="${NPM_CONFIG_CACHE:-$(mktemp -d)}"

current_version=$(jq -r '.version' "$sources_file")

# Latest version from the npm registry
version=$(curl -fsSL "https://registry.npmjs.org/pi-fabric/latest" | jq -r '.version')
tarball="https://registry.npmjs.org/pi-fabric/-/pi-fabric-${version}.tgz"

if [ "$current_version" = "$version" ]; then
  echo "Already at latest version: $version"
fi

echo "Updating from $current_version to $version"

src_hash=$(nix hash convert --hash-algo sha256 --to sri \
  "$(nix-prefetch-url --type sha256 "$tarball" 2>/dev/null | tail -1)")

tmpdir=$(mktemp -d)
trap 'rm -rf "$tmpdir"' EXIT

curl -fsSL "$tarball" -o "$tmpdir/pifab.tgz"
tar xzf "$tmpdir/pifab.tgz" -C "$tmpdir"

# Strip peer and dev deps so npm does not try to resolve the Pi packages that
# are not on the npm registry, and drop scripts (we never build in Nix).
node -e "
  const fs = require('fs');
  const pj = JSON.parse(fs.readFileSync('$tmpdir/package/package.json', 'utf8'));
  delete pj.peerDependencies;
  delete pj.devDependencies;
  delete pj.scripts;
  fs.writeFileSync('$pkgjson', JSON.stringify(pj, null, 2) + '\n');
"

cp "$pkgjson" "$tmpdir/package/package.json"
(cd "$tmpdir/package" && npm install --package-lock-only --ignore-scripts --no-audit --no-fund)
cp "$tmpdir/package/package-lock.json" "$lockfile"

jq -n \
  --arg version "$version" \
  --arg srcHash "$src_hash" \
  --arg npmDepsHash "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=" \
  '{version: $version, srcHash: $srcHash, npmDepsHash: $npmDepsHash}' >"$sources_file"

build_log=$(nix-build -E \
  "let pkgs = import <nixpkgs> {}; in pkgs.callPackage $script_dir/package.nix {}" \
  2>&1 || true)
npm_deps_hash=$(echo "$build_log" | grep 'got:' | head -1 | sed 's/.*got: *//')

if [ -z "$npm_deps_hash" ]; then
  echo "ERROR: Failed to determine npmDeps hash"
  nix-build -E "let pkgs = import <nixpkgs> {}; in pkgs.callPackage $script_dir/package.nix {}" 2>&1 | tail -10
  exit 1
fi

jq -n \
  --arg version "$version" \
  --arg srcHash "$src_hash" \
  --arg npmDepsHash "$npm_deps_hash" \
  '{version: $version, srcHash: $srcHash, npmDepsHash: $npmDepsHash}' >"$sources_file"

echo "Verifying build..."
nix-build -E \
  "let pkgs = import <nixpkgs> {}; in pkgs.callPackage $script_dir/package.nix {}" \
  --no-out-link

echo "Done. Updated to $version"
