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

# Fetch all releases
all_releases=$(curl -fsSL "$API_URL?per_page=100")

current=$(cat "$sources_file")

# For each platform, find the latest release that has a matching asset
for nix_sys in "${!NIX_PLATFORMS[@]}"; do
  platform_type="${NIX_PLATFORMS[$nix_sys]}"

  echo "Resolving version for $nix_sys ($platform_type)..."

  # Iterate through releases (newest first) to find one with a matching asset
  found_version=""
  found_idx=0
  release_count=$(printf '%s' "$all_releases" | jaq '. | length')
  while [ "$found_idx" -lt "$release_count" ] && [ -z "$found_version" ]; do
    release_json=$(printf '%s' "$all_releases" | jaq ".[$found_idx]")
    candidate=$(printf '%s' "$release_json" | jaq -r '.tag_name | ltrimstr("v")')

    # Determine expected asset name
    if [ "$platform_type" = "appimage" ]; then
      arch=$(printf '%s' "$nix_sys" | cut -d- -f1)
      file="Mouseless_v${candidate}_debian-13_${arch}.AppImage"
    else
      file="mouseless-installer_v${candidate}.dmg"
    fi

    # Check if this asset exists in the release
    has_asset=$(printf '%s' "$release_json" | jaq --arg f "$file" '[.assets[].name] | any(. == $f)')
    if [ "$has_asset" = "true" ]; then
      found_version="$candidate"
      echo "  Found version $found_version for $nix_sys"
    else
      echo "  No asset '$file' in release $candidate, trying next..."
      found_idx=$((found_idx + 1))
    fi
  done

  if [ -z "$found_version" ]; then
    echo "WARNING: could not find any release with assets for $nix_sys; keeping existing entry"
    continue
  fi

  # Check if this platform is already at the found version
  current_version=$(printf '%s' "$current" | jaq -r --arg s "$nix_sys" '.[$s].version // ""')
  current_url=$(printf '%s' "$current" | jaq -r --arg s "$nix_sys" '.[$s].url // ""')
  url="$BASE_URL/download/v${found_version}/$file"

  if [ "$current_version" = "$found_version" ] && [ "$current_url" = "$url" ]; then
    echo "  $nix_sys already at $found_version, skipping"
    continue
  fi

  echo "  Fetching hash for $nix_sys: $file"
  raw_hash=$(nix-prefetch-url --type sha256 "$url" 2>/dev/null | tail -1) || {
    echo "WARNING: failed to fetch $url; keeping existing entry for $nix_sys"
    continue
  }
  hash=$(nix hash convert --hash-algo sha256 --to sri "$raw_hash")

  result=$(printf '%s' "$current" \
    | jaq --arg s "$nix_sys" --arg v "$found_version" --arg u "$url" --arg h "$hash" \
      '.[$s] = {version: $v, url: $u, hash: $h}')
  current="$result"
done

printf '%s\n' "$current" | jaq '.' > "$sources_file"
echo "Done. Updated sources.json"
