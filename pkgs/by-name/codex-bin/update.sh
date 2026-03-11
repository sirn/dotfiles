#!/usr/bin/env nix-shell
#!nix-shell -i bash --packages coreutils curl jq nix gnused
set -euo pipefail

script_dir="$(dirname "${BASH_SOURCE[0]}")"
package_file="$script_dir/package.nix"

current_version=$(sed -n 's/^\s*version = "\([^"]*\)";$/\1/p' "$package_file" | head -1)
version=$(curl -s https://api.github.com/repos/openai/codex/releases/latest | jq -r '.tag_name' | sed 's/^rust-v//')

if [ "$current_version" = "$version" ]; then
  echo "Already at latest version: $version"
  exit 0
fi

echo "Updating from $current_version to $version"

declare -A platforms=(
  ["x86_64-linux"]="x86_64-unknown-linux-musl"
  ["aarch64-linux"]="aarch64-unknown-linux-musl"
  ["aarch64-darwin"]="aarch64-apple-darwin"
  ["x86_64-darwin"]="x86_64-apple-darwin"
)

sed -i "s/version = \"$current_version\";/version = \"$version\";/" "$package_file"

for system in "${!platforms[@]}"; do
  target="${platforms[$system]}"
  url="https://github.com/openai/codex/releases/download/rust-v${version}/codex-${target}.tar.gz"
  echo "Fetching hash for $system ($target)..."
  hash=$(nix-prefetch-url --type sha256 "$url" 2>/dev/null)
  sri=$(nix hash to-sri --type sha256 "$hash" 2>/dev/null)
  echo "  $system: $sri"
  sed -i "/target = \"$target\";/,/hash = \"sha256-/ { s|hash = \"sha256-[^\"]*\"|hash = \"$sri\"| }" "$package_file"
done

echo "Updated $package_file to version $version"
