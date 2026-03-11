#!/usr/bin/env nix-shell
#!nix-shell -i bash --packages coreutils curl jq nix gnused
set -euo pipefail

script_dir="$(dirname "${BASH_SOURCE[0]}")"
package_file="$script_dir/package.nix"

base_url="https://storage.googleapis.com/claude-code-dist-86c565f3-f756-42ad-8dfa-d59b1c096819/claude-code-releases"

current_version=$(sed -n 's/^\s*version = "\([^"]*\)";$/\1/p' "$package_file" | head -1)
version=$(curl -s "${base_url}/latest")

if [ "$current_version" = "$version" ]; then
  echo "Already at latest version: $version"
  exit 0
fi

echo "Updating from $current_version to $version"

manifest=$(curl -s "${base_url}/${version}/manifest.json")

declare -A platforms=(
  ["x86_64-linux"]="linux-x64"
  ["aarch64-linux"]="linux-arm64"
  ["aarch64-darwin"]="darwin-arm64"
  ["x86_64-darwin"]="darwin-x64"
)

sed -i "s/version = \"$current_version\";/version = \"$version\";/" "$package_file"

for system in "${!platforms[@]}"; do
  platform="${platforms[$system]}"
  hex=$(echo "$manifest" | jq -r ".platforms.\"${platform}\".checksum")
  sri=$(nix hash to-sri --type sha256 "$hex")
  echo "  $system ($platform): $sri"
  sed -i "/platform = \"$platform\";/,/hash = \"sha256-/ { s|hash = \"sha256-[^\"]*\"|hash = \"$sri\"| }" "$package_file"
done

echo "Updated $package_file to version $version"
