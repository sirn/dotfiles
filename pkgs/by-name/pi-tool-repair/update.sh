#!/usr/bin/env nix-shell
#!nix-shell -i bash --packages coreutils curl jq nix cacert
set -euo pipefail

script_dir="$(dirname "${BASH_SOURCE[0]}")"
sources_file="$script_dir/sources.json"

if [ -n "${NIX_SSL_CERT_FILE:-}" ] && [ -f "${NIX_SSL_CERT_FILE}" ]; then
  export SSL_CERT_FILE="${NIX_SSL_CERT_FILE}"
fi

current_version=$(jq -r '.version' "$sources_file")

# Fetch latest version from npm-shrinkwrap on main
version=$(curl -sL "https://raw.githubusercontent.com/monotykamary/pi-tool-repair/main/package.json" | jq -r '.version')
commit=$(curl -s "https://api.github.com/repos/monotykamary/pi-tool-repair/commits/main" | jq -r '.sha')

if [ "$current_version" = "$version" ]; then
  echo "Already at latest version: $version"
else
  echo "Updating from $current_version to $version (commit: $commit)"
fi

# Prefetch source tarball using commit hash
src_hash=$(nix hash convert --hash-algo sha256 --to sri \
  "$(nix-prefetch-url --unpack \
    "https://github.com/monotykamary/pi-tool-repair/archive/${commit}.tar.gz" \
    2>/dev/null | tail -1)")

# Write sources.json
jq -n \
  --arg version "$version" \
  --arg rev "$commit" \
  --arg srcHash "$src_hash" \
  '{version: $version, rev: $rev, srcHash: $srcHash}' >"$sources_file"

echo "Verifying build..."
nix-build -E \
  "let pkgs = import <nixpkgs> {}; in pkgs.callPackage $script_dir/package.nix {}" \
  --no-out-link

echo "Done. Updated to $version (commit: $commit)"
