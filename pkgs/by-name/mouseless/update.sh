#!/usr/bin/env nix-shell
#!nix-shell -i bash --packages coreutils curl jaq nix cacert
set -euo pipefail

script_dir="$(dirname "${BASH_SOURCE[0]}")"
sources_file="$script_dir/sources.json"

if [ -n "${NIX_SSL_CERT_FILE:-}" ] && [ -f "${NIX_SSL_CERT_FILE:-}" ]; then
  export SSL_CERT_FILE="${NIX_SSL_CERT_FILE}"
fi

BASE_URL="https://github.com/cymian/mouseless/releases"
API_URL="https://api.github.com/repos/cymian/mouseless/releases"

# Platform mapping: Nix system -> type
# For Linux: debian-13 AppImage; for Darwin: DMG installer
declare -A NIX_PLATFORMS
NIX_PLATFORMS=(
  ["x86_64-linux"]="appimage"
  ["aarch64-linux"]="appimage"
  ["aarch64-darwin"]="dmg"
)

# Fetch release info
if [ -n "${1:-}" ]; then
  version="$1"
  echo "Using specified version: $version"
  release_json=$(curl -fsSL "$API_URL/tags/v${version}")
else
  release_json=$(curl -fsSL "$API_URL/latest")
  version=$(printf '%s' "$release_json" | jaq -r '.tag_name | ltrimstr("v")')
fi

current=$(cat "$sources_file")
current_version=$(printf '%s' "$current" | jaq -r '.version')

if [ "$current_version" = "$version" ]; then
  echo "Already at latest version: $version"
  exit 0
fi

echo "Updating from $current_version to $version"

# Extract asset names from the release for membership checks
asset_names=$(printf '%s' "$release_json" | jaq '[.assets[].name]')

# Start from current sources and update the version
result=$(printf '%s' "$current" | jaq --arg v "$version" '.version = $v')

for nix_sys in "${!NIX_PLATFORMS[@]}"; do
  platform_type="${NIX_PLATFORMS[$nix_sys]}"

  if [ "$platform_type" = "appimage" ]; then
    arch=$(printf '%s' "$nix_sys" | cut -d- -f1)
    file="Mouseless_v${version}_debian-13_${arch}.AppImage"
  else
    file="mouseless-installer_v${version}.dmg"
  fi

  url="$BASE_URL/download/v${version}/$file"

  # Check if this asset exists in the release
  has_asset=$(printf '%s' "$asset_names" | jaq -r --arg f "$file" 'any(. == $f)')
  if [ "$has_asset" != "true" ]; then
    echo "WARNING: no asset '$file' in release $version; keeping existing entry for $nix_sys"
    continue
  fi

  # Skip if URL is unchanged (avoids re-downloading)
  current_url=$(printf '%s' "$current" | jaq -r --arg s "$nix_sys" '.[$s].url // ""')
  if [ "$current_url" = "$url" ]; then
    echo "  $nix_sys already at $version, skipping"
    continue
  fi

  echo "  Fetching hash for $nix_sys: $file"
  raw_hash=$(nix-prefetch-url --type sha256 "$url" 2>/dev/null | tail -1) || {
    echo "WARNING: failed to fetch $url; keeping existing entry for $nix_sys"
    continue
  }
  hash=$(nix hash convert --hash-algo sha256 --to sri "$raw_hash")

  result=$(printf '%s' "$result" \
    | jaq --arg s "$nix_sys" --arg u "$url" --arg h "$hash" \
      '.[$s] = {url: $u, hash: $h}')
done

printf '%s\n' "$result" | jaq '.' > "$sources_file"
echo "Done. Updated to $version"
