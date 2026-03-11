#!/usr/bin/env nix-shell
#!nix-shell -i bash --packages coreutils curl jq nix gnused
set -euo pipefail

script_dir="$(dirname "${BASH_SOURCE[0]}")"
package_file="$script_dir/package.nix"

current_version=$(sed -n 's/^\s*version = "\([^"]*\)";$/\1/p' "$package_file" | head -1)
version=$(curl -s https://api.github.com/repos/anomalyco/opencode/releases/latest | jq -r '.tag_name' | sed 's/^v//')

if [ "$current_version" = "$version" ]; then
  echo "Already at latest version: $version"
  exit 0
fi

echo "Updating from $current_version to $version"

declare -A platforms=(
  ["x86_64-linux"]="linux-x64:tar.gz"
  ["aarch64-linux"]="linux-arm64:tar.gz"
  ["aarch64-darwin"]="darwin-arm64:zip"
  ["x86_64-darwin"]="darwin-x64:zip"
)

sed -i "s/version = \"$current_version\";/version = \"$version\";/" "$package_file"

for system in "${!platforms[@]}"; do
  IFS=':' read -r arch ext <<< "${platforms[$system]}"
  url="https://github.com/anomalyco/opencode/releases/download/v${version}/opencode-${arch}.${ext}"
  echo "Fetching hash for $system ($arch)..."
  hash=$(nix-prefetch-url --type sha256 "$url" 2>/dev/null)
  sri=$(nix hash to-sri --type sha256 "$hash" 2>/dev/null)
  echo "  $system: $sri"
  sed -i "/arch = \"$arch\";/,/hash = \"sha256-/ { s|hash = \"sha256-[^\"]*\"|hash = \"$sri\"| }" "$package_file"
done

echo "Updated $package_file to version $version"
