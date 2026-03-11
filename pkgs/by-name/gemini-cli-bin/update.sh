#!/usr/bin/env nix-shell
#!nix-shell -i bash --packages coreutils curl jq nix gnused
set -euo pipefail

script_dir="$(dirname "${BASH_SOURCE[0]}")"
package_file="$script_dir/package.nix"

current_version=$(sed -n 's/^\s*version = "\([^"]*\)";$/\1/p' "$package_file" | head -1)
version=$(curl -s https://api.github.com/repos/google-gemini/gemini-cli/releases/latest | jq -r '.tag_name' | sed 's/^v//')

if [ "$current_version" = "$version" ]; then
  echo "Already at latest version: $version"
  exit 0
fi

echo "Updating from $current_version to $version"

url="https://github.com/google-gemini/gemini-cli/releases/download/v${version}/gemini.js"
echo "Fetching hash for gemini.js..."
hash=$(nix-prefetch-url --type sha256 "$url" 2>/dev/null)
sri=$(nix hash to-sri --type sha256 "$hash" 2>/dev/null)
echo "  $sri"

sed -i "s/version = \"$current_version\";/version = \"$version\";/" "$package_file"
sed -i "s|hash = \"sha256-[^\"]*\"|hash = \"$sri\"|" "$package_file"

echo "Updated $package_file to version $version"
