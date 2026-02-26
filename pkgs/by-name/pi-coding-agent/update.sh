#!/usr/bin/env nix-shell
#!nix-shell -i bash --packages curl jq nix-update gnused
set -euo pipefail

script_dir="$(dirname "${BASH_SOURCE[0]}")"
current_version=$(sed -n 's/^\s*version = "\([^"]*\)";$/\1/p' "$script_dir/package.nix" | head -1)

# Get latest version from GitHub releases API
version=$(curl -s https://api.github.com/repos/badlogic/pi-mono/releases/latest | jq -r '.tag_name' | sed 's/^v//')

if [ "$current_version" = "$version" ]; then
  echo "Already at latest version: $version"
  exit 0
fi

echo "Updating from $current_version to $version"
nix-update local.pi-coding-agent --version="$version"
