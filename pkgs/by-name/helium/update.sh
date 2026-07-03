#!/usr/bin/env nix-shell
#!nix-shell -i bash --packages coreutils curl jq nix cacert nix-prefetch-git common-updater-scripts
set -euo pipefail

script_dir="$(dirname "${BASH_SOURCE[0]}")"
sources_file="$script_dir/sources.json"
repo="imputnet/helium-linux"

if [ -n "${NIX_SSL_CERT_FILE:-}" ] && [ -f "${NIX_SSL_CERT_FILE}" ]; then
  export SSL_CERT_FILE="${NIX_SSL_CERT_FILE}"
fi

# Latest tag from the upstream GitHub repository. Tags are bare version
# numbers (e.g. "0.14.2.1") with no leading "v".
latest_tag=$(list-git-tags --url="https://github.com/${repo}" |
  grep -v '\^{}' | sort -V | tail -1)
version="${latest_tag}"

current_version=$(sed -n 's/.*version = "\([^"]*\)".*/\1/p' "$script_dir/package.nix" | head -1)

if [ "$current_version" = "$version" ]; then
  echo "Already at latest version: $version"
  exit 0
fi

echo "Updating from $current_version to $version"

update_hash() {
  local system="$1" suffix="$2"
  local url="https://github.com/${repo}/releases/download/${version}/helium-bin_${version}-1_${suffix}.deb"
  local hash
  hash=$(nix hash convert --hash-algo sha256 --to sri \
    "$(nix-prefetch-url --type sha256 "$url" 2>/dev/null | tail -1)")
  jq --arg system "$system" --arg hash "$hash" \
    '.[$system] = $hash' "$sources_file" >"$sources_file.tmp"
  mv "$sources_file.tmp" "$sources_file"
}

update_hash x86_64-linux amd64
update_hash aarch64-linux arm64

# Bump the version in package.nix.
sed -i.bak "s|version = \"$current_version\"|version = \"$version\"|" "$script_dir/package.nix"
rm -f "$script_dir/package.nix.bak"

echo "Done. Updated to $version"
