#!/usr/bin/env nix-shell
#!nix-shell -i bash --packages coreutils curl jq nix
set -euo pipefail

script_dir="$(dirname "${BASH_SOURCE[0]}")"
sources_file="$script_dir/sources.json"

current_version=$(jq -r '.version' "$sources_file")
version=$(curl -s https://api.github.com/repos/badlogic/pi-mono/releases/latest | jq -r '.tag_name' | sed 's/^v//')

if [ "$current_version" = "$version" ]; then
    echo "Already at latest version: $version"
    exit 0
fi

echo "Updating from $current_version to $version"

declare -A platforms=(
    ["x86_64-linux"]="linux-x64"
    ["aarch64-linux"]="linux-arm64"
    ["aarch64-darwin"]="darwin-arm64"
    ["x86_64-darwin"]="darwin-x64"
)

declare -A system_entries
for system in "${!platforms[@]}"; do
    arch="${platforms[$system]}"
    url="https://github.com/badlogic/pi-mono/releases/download/v${version}/pi-${arch}.tar.gz"
    echo "Fetching hash for $system ($arch)..."
    hash=$(nix-prefetch-url --type sha256 "$url" 2>/dev/null)
    sri=$(nix hash convert --hash-algo sha256 --to sri "$hash" 2>/dev/null)
    echo "  $system: $sri"
    system_entries[$system]=$(jq -n --arg url "$url" --arg hash "$sri" '{url: $url, hash: $hash}')
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
