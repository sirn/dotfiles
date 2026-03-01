#!/usr/bin/env nix-shell
#!nix-shell -i bash --packages coreutils curl jq nix gnused
set -euo pipefail

script_dir="$(dirname "${BASH_SOURCE[0]}")"
package_file="$script_dir/package.nix"

# Get current version
current_version=$(sed -n 's/^\s*version = "\([^"]*\)";$/\1/p' "$package_file" | head -1)

# Get latest version from GitHub releases API
version=$(curl -s https://api.github.com/repos/badlogic/pi-mono/releases/latest | jq -r '.tag_name' | sed 's/^v//')

if [ "$current_version" = "$version" ]; then
  echo "Already at latest version: $version"
  exit 0
fi

echo "Updating from $current_version to $version"

# Platform definitions: nix system -> (arch in filename, attribute name in platformMap)
declare -A platforms=(
  ["x86_64-linux"]="linux-x64"
  ["aarch64-linux"]="linux-arm64"
  ["aarch64-darwin"]="darwin-arm64"
  ["x86_64-darwin"]="darwin-x64"
)

# Update version in package.nix
sed -i "s/version = \"$current_version\";/version = \"$version\";/" "$package_file"

# Fetch and update hash for each platform
for system in "${!platforms[@]}"; do
  arch="${platforms[$system]}"
  url="https://github.com/badlogic/pi-mono/releases/download/v${version}/pi-${arch}.tar.gz"

  echo "Fetching hash for $system ($arch)..."
  hash=$(nix-prefetch-url --type sha256 "$url" 2>/dev/null)

  # Convert to SRI format (sha256-xxx)
  sri_hash=$(nix hash to-sri --type sha256 "$hash" 2>/dev/null)

  echo "  $system: $sri_hash"

  # Update the hash in package.nix using sed
  # Match the line with this system's arch and replace the hash
  sed -i "/arch = \"$arch\";/,/hash = \"sha256:/ { s|hash = \"sha256:[^\"]*\"|hash = \"$sri_hash\"| }" "$package_file"
done

echo "Updated $package_file to version $version"
