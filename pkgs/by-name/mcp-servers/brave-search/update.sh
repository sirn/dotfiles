#!/usr/bin/env nix-shell
#!nix-shell -i bash --packages nodejs nix-update git
set -euo pipefail
version=$(npm view @brave/brave-search-mcp-server version)

cd "$(dirname "${BASH_SOURCE[@]}")"
npm i --package-lock-only @brave/brave-search-mcp-server@"$version"
rm package.json

cd -
nix-update local.mcpServers.brave-search --version="$version"
