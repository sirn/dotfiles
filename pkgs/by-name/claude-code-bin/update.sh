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

declare -A system_entries
for system in "${!platforms[@]}"; do
    platform="${platforms[$system]}"
    hex=$(echo "$manifest" | jq -r ".platforms.\"${platform}\".checksum")
    sri=$(nix hash convert --hash-algo sha256 --to sri "$hex")
    url="${base_url}/${version}/${platform}/claude"
    system_entries[$system]=$(jq -n --arg url "$url" --arg hash "$sri" '{url: $url, hash: $hash}')
    echo "  $system ($platform): $sri"
done

# Build systems JSON
systems_json=$(
    printf '%s\n' "${system_entries[@]}" |
        jq -s '.' |
        jq '{"x86_64-linux": .[0], "aarch64-linux": .[1], "aarch64-darwin": .[2], "x86_64-darwin": .[3]}'
)

jq -n \
    --arg version "$version" \
    --argjson systems "$systems_json" \
    '{version: $version, systems: $systems}' >"$sources_file"

echo "Updated $sources_file to version $version"
