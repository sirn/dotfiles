#!/usr/bin/env nix-shell
#!nix-shell -i bash --packages coreutils curl jq nix
set -euo pipefail

script_dir="$(dirname "${BASH_SOURCE[0]}")"
sources_file="$script_dir/sources.json"

current_version=$(jq -r '.version' "$sources_file")
version=$(curl -s https://api.github.com/repos/google-gemini/gemini-cli/releases/latest | jq -r '.tag_name' | sed 's/^v//')

if [ "$current_version" = "$version" ]; then
    echo "Already at latest version: $version"
    exit 0
fi

echo "Updating from $current_version to $version"

# Generate URLs
gemini_url="https://github.com/google-gemini/gemini-cli/releases/download/v${version}/gemini.js"
base_url="https://raw.githubusercontent.com/google-gemini/gemini-cli/v${version}/packages/cli/src/utils"

# Fetch gemini.js hash
echo "Fetching hash for gemini.js..."
hash=$(nix-prefetch-url --type sha256 "$gemini_url" 2>/dev/null)
gemini_hash=$(nix hash convert --hash-algo sha256 --to sri "$hash" 2>/dev/null)
echo "  $gemini_hash"

# Fetch sandbox hashes
declare -a file_entries
for file in \
    sandbox-macos-permissive-open.sb \
    sandbox-macos-permissive-proxied.sb \
    sandbox-macos-restrictive-open.sb \
    sandbox-macos-restrictive-proxied.sb \
    sandbox-macos-strict-open.sb \
    sandbox-macos-strict-proxied.sb; do
    url="${base_url}/${file}"
    echo "Fetching hash for ${file}..."
    hash=$(nix-prefetch-url --type sha256 "$url" 2>/dev/null)
    sri=$(nix hash convert --hash-algo sha256 --to sri "$hash" 2>/dev/null)
    echo "  $sri"
    file_entries+=("$(jq -n --arg url "$url" --arg hash "$sri" '{url: $url, hash: $hash}')")
done

# Build final JSON
printf '%s\n' "${file_entries[@]}" | jq -s '.' >"/tmp/files.json"
jq -n \
    --arg version "$version" \
    --arg url "$gemini_url" \
    --arg hash "$gemini_hash" \
    --slurpfile files "/tmp/files.json" \
    '{version: $version, src: {url: $url, hash: $hash}, files: $files[0]}' >"$sources_file"
rm -f "/tmp/files.json"

echo "Updated $sources_file to $version"
