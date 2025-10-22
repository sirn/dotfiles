#!/usr/bin/env nix-shell
#!nix-shell -i bash --packages nodejs nix-update git gnused
set -euo pipefail

script_dir="$(dirname "${BASH_SOURCE[@]}")"
current_version=$(sed -n 's/^  version = "\(.*\)";$/\1/p' "$script_dir/package.nix")
version=$(npm view @brave/brave-search-mcp-server version)

if [ "$current_version" = "$version" ]; then
  exit 0
fi

cd "$script_dir"
npm i --package-lock-only @brave/brave-search-mcp-server@"$version"
rm package.json

cd -
nix-update local.mcpServers.brave-search --version="$version"
