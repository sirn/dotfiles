#!/usr/bin/env nix-shell
#!nix-shell -i bash --packages coreutils curl jq nix
set -euo pipefail

script_dir="$(dirname "${BASH_SOURCE[0]}")"
sources_file="$script_dir/sources.json"

current_version=$(jq -r '.version' "$sources_file")
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

systems_json="{}"
for system in "${!platforms[@]}"; do
    IFS=':' read -r arch ext <<<"${platforms[$system]}"
    url="https://github.com/anomalyco/opencode/releases/download/v${version}/opencode-${arch}.${ext}"
    echo "Fetching hash for $system ($arch)..."
    hash=$(nix-prefetch-url --type sha256 "$url" 2>/dev/null)
    sri=$(nix hash convert --hash-algo sha256 --to sri "$hash" 2>/dev/null)
    echo "  $system: $sri"
    systems_json=$(echo "$systems_json" | jq --arg key "$system" --arg url "$url" --arg hash "$sri" \
        '. + {($key): {url: $url, hash: $hash}}')
done

jq -n \
    --arg version "$version" \
    --argjson systems "$systems_json" \
    '{version: $version, systems: $systems}' >"$sources_file"

echo "Updated $sources_file to version $version"
