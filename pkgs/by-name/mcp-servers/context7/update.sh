#!/usr/bin/env nix-shell
#!nix-shell -i bash --packages nodejs nix-update git
set -euo pipefail
version=$(npm view @upstash/context7-mcp version)

cd "$(dirname "${BASH_SOURCE[@]}")"
npm i --package-lock-only @upstash/context7-mcp@"$version"
rm package.json

cd -
nix-update local.mcpServers.context7 --version="$version"
