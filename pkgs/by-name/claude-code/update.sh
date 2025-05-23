#!/usr/bin/env nix-shell
#!nix-shell -i bash -p nodePackages.npm -p coreutils -p prefetch-npm-deps -p jq

BASE_DIR=$(
    cd "$(dirname "$0")" || exit
    pwd -P
)

## Prelude
## ----------------------------------------------------------------------------

cd "${BASE_DIR}" || exit 1
VERSION=$(npm view @anthropic-ai/claude-code version)

## NPM
## ----------------------------------------------------------------------------

echo "Updating package-lock.json..."
npm i --package-lock-only @anthropic-ai/claude-code@"$VERSION" >/dev/null
rm -f package.json

## Nix
## ----------------------------------------------------------------------------

compute_nixpkgs_hash() {
    url=$1
    hash=$(nix-prefetch-url --unpack --type sha256 "$1" 2>/dev/null)
    nix hash to-sri --type sha256 "$hash"
}

compute_npmdeps_hash() {
    lockfile=$1
    prefetch-npm-deps "$lockfile"
}

echo "Fetching hash..."
HASH=$(compute_nixpkgs_hash "https://registry.npmjs.org/@anthropic-ai/claude-code/-/claude-code-${VERSION}.tgz")
NPMDEPSHASH=$(compute_npmdeps_hash "package-lock.json")
echo "  version: ${VERSION}"
echo "  hash: ${HASH}"
echo "  npmDepsHash: ${NPMDEPSHASH}"

echo "Updating default.nix..."
sed -i -E "
    s|(version =).*|\1 \"${VERSION}\";|
    s|(hash =).*|\1 \"${HASH}\";|
    s|(npmDepsHash =).*|\1 \"${NPMDEPSHASH}\";|" \
    default.nix

echo "Done"
