#!/usr/bin/env nix-shell
#!nix-shell -i bash --packages nodejs nix-update git
set -euo pipefail
version=$(npm view octofriend version)

cd "$(dirname "${BASH_SOURCE[@]}")"
npm i --package-lock-only octofriend@"$version"
rm package.json

cd -
nix-update local.octofriend --version="$version"
