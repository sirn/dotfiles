#!/usr/bin/env nix-shell
#!nix-shell -i bash --packages coreutils curl jq nix
set -euo pipefail

script_dir="$(dirname "${BASH_SOURCE[0]}")"
sources_file="$script_dir/sources.json"

base_url="https://storage.googleapis.com/claude-code-dist-86c565f3-f756-42ad-8dfa-d59b1c096819/claude-code-releases"

current_version=$(jq -r '.version' "$sources_file")
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

systems_json="{}"
for system in "${!platforms[@]}"; do
    platform="${platforms[$system]}"
    hex=$(echo "$manifest" | jq -r ".platforms.\"${platform}\".checksum")
    sri=$(nix hash convert --hash-algo sha256 --to sri "$hex")
    url="${base_url}/${version}/${platform}/claude"
    systems_json=$(echo "$systems_json" | jq --arg key "$system" --arg url "$url" --arg hash "$sri" \
        '. + {($key): {url: $url, hash: $hash}}')
    echo "  $system ($platform): $sri"
done

jq -n \
    --arg version "$version" \
    --argjson systems "$systems_json" \
    '{version: $version, systems: $systems}' >"$sources_file"

echo "Updated $sources_file to version $version"
