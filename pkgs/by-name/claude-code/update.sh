#!/usr/bin/env nix-shell
#!nix-shell -i bash --packages coreutils curl jaq nix cacert
set -euo pipefail

script_dir="$(dirname "${BASH_SOURCE[0]}")"
sources_file="$script_dir/sources.json"

if [ -n "${NIX_SSL_CERT_FILE:-}" ] && [ -f "${NIX_SSL_CERT_FILE:-}" ]; then
  export SSL_CERT_FILE="${NIX_SSL_CERT_FILE}"
fi

BASE_URL="https://storage.googleapis.com/claude-code-dist-86c565f3-f756-42ad-8dfa-d59b1c096819/claude-code-releases"

current_version=$(jaq -r '.version' "$sources_file")
version="${1:-$(curl -fsSL "$BASE_URL/latest")}"

if [ "$current_version" = "$version" ]; then
  echo "Already at latest version: $version"
  exit 0
fi

echo "Updating from $current_version to $version"

manifest=$(curl -fsSL "$BASE_URL/$version/manifest.json")

# Platform mapping: Nix system -> node platform key
declare -A NIX_TO_NODE=(
  [aarch64-darwin]=darwin-arm64
  [x86_64-darwin]=darwin-x64
  [aarch64-linux]=linux-arm64
  [x86_64-linux]=linux-x64
)

# Build sources.json by fetching each binary to compute its nix hash
tmp=$(mktemp)
trap 'rm -f "$tmp"' EXIT

printf '{\n  "version": "%s"' "$version" >"$sources_file"

for nix_sys in "${!NIX_TO_NODE[@]}"; do
  node_key="${NIX_TO_NODE[$nix_sys]}"
  url="$BASE_URL/$version/$node_key/claude"
  hash=$(nix hash convert --hash-algo sha256 --to sri \
    "$(nix-prefetch-url --type sha256 "$url" 2>/dev/null | tail -1)")

  printf ',\n  "%s": {\n    "url": "%s",\n    "hash": "%s"\n  }' \
    "$nix_sys" "$url" "$hash" >>"$sources_file"
done

printf '\n}\n' >>"$sources_file"

echo "Verifying build..."
nix-build -E \
  'let pkgs = import <nixpkgs> {}; in pkgs.callPackage ./pkgs/by-name/claude-code/package.nix {}' \
  --no-out-link 2>&1 | tail -5 || true

echo "Done. Updated to $version"
