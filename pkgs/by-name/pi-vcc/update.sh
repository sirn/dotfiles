#!/usr/bin/env nix-shell
#!nix-shell -i bash --packages coreutils curl jq nodejs nix cacert
set -euo pipefail

script_dir="$(dirname "${BASH_SOURCE[0]}")"
sources_file="$script_dir/sources.json"
lockfile="$script_dir/package-lock.json"

if [ -n "${NIX_SSL_CERT_FILE:-}" ] && [ -f "${NIX_SSL_CERT_FILE}" ]; then
  export SSL_CERT_FILE="${NIX_SSL_CERT_FILE}"
  export NPM_CONFIG_CAFILE="${NIX_SSL_CERT_FILE}"
fi

current_version=$(jq -r '.version' "$sources_file")

# Fetch latest package.json from tom to get version
version=$(curl -sL "https://raw.githubusercontent.com/monotykamary/pi-vcc/tom/package.json" | jq -r '.version')
# Get the commit hash for the source tarball
commit=$(curl -s "https://api.github.com/repos/monotykamary/pi-vcc/commits/tom" | jq -r '.sha')

if [ "$current_version" = "$version" ]; then
  echo "Already at latest version: $version, ensuring npm deps hash is up to date"
fi

echo "Updating from $current_version to $version (commit: $commit)"

# Prefetch source tarball using commit hash
src_hash=$(nix hash convert --hash-algo sha256 --to sri \
  "$(nix-prefetch-url --unpack \
    "https://github.com/monotykamary/pi-vcc/archive/${commit}.tar.gz" \
    2>/dev/null | tail -1)")

# Download package.json and generate package-lock.json
tmpdir=$(mktemp -d)
trap 'rm -rf "$tmpdir"' EXIT

curl -sL "https://raw.githubusercontent.com/monotykamary/pi-vcc/${commit}/package.json" \
  >"$tmpdir/package.json"

# Strip peer/dev deps (same as postPatch in package.nix)
node -e "
  const pj = JSON.parse(require('fs').readFileSync('$tmpdir/package.json','utf8'));
  delete pj.peerDependencies;
  delete pj.devDependencies;
  require('fs').writeFileSync('$tmpdir/package.json', JSON.stringify(pj, null, 2) + '\n');
"

(cd "$tmpdir" && npm install --package-lock-only --ignore-scripts)
cp "$tmpdir/package-lock.json" "$lockfile"

# Write sources.json with fake npmDepsHash, then build to get the real one
jq -n \
  --arg version "$version" \
  --arg rev "$commit" \
  --arg srcHash "$src_hash" \
  --arg npmDepsHash "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=" \
  '{version: $version, rev: $rev, srcHash: $srcHash, npmDepsHash: $npmDepsHash}' >"$sources_file"

build_log=$(nix-build -E \
  "let pkgs = import <nixpkgs> {}; in pkgs.callPackage $script_dir/package.nix {}" \
  2>&1 || true)
npm_deps_hash=$(echo "$build_log" | grep 'got:' | head -1 | sed 's/.*got: *//')

if [ -z "$npm_deps_hash" ]; then
  echo "ERROR: Failed to determine npmDeps hash"
  nix-build -E "let pkgs = import <nixpkgs> {}; in pkgs.callPackage $script_dir/package.nix {}" 2>&1 | tail -10
  exit 1
fi

# Write final sources.json with the real npmDepsHash
jq -n \
  --arg version "$version" \
  --arg rev "$commit" \
  --arg srcHash "$src_hash" \
  --arg npmDepsHash "$npm_deps_hash" \
  '{version: $version, rev: $rev, srcHash: $srcHash, npmDepsHash: $npmDepsHash}' >"$sources_file"

echo "Verifying build..."

nix-build -E \
  "let pkgs = import <nixpkgs> {}; in pkgs.callPackage $script_dir/package.nix {}" \
  --no-out-link

echo "Done. Updated to $version (commit: $commit)"
