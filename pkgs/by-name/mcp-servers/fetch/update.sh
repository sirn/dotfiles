#!/usr/bin/env nix-shell
#!nix-shell -i bash -p curl -p coreutils -p jq

BASE_DIR=$(
    cd "$(dirname "$0")" || exit
    pwd -P
)

## Prelude
## ----------------------------------------------------------------------------

cd "${BASE_DIR}" || exit 1
PACKAGE_NAME=mcp-server-fetch

echo "Fetching package info from PyPI..."
PACKAGE_INFO=$(curl -s "https://pypi.org/pypi/$PACKAGE_NAME/json")
VERSION=$(echo "$PACKAGE_INFO" | jq -r '.info.version')
PACKAGE_URL=$(echo "$PACKAGE_INFO" | jq -r '.urls[] | select(.packagetype == "sdist") | .url')

## Nix
## ----------------------------------------------------------------------------

compute_nixpkgs_hash() {
    url=$1
    hash=$(nix-prefetch-url --type sha256 "$1" 2>/dev/null)
    nix hash convert --hash-algo sha256 --from nix32 --to sri "$hash" 2>/dev/null
}

echo "Fetching hash..."
HASH=$(compute_nixpkgs_hash "$PACKAGE_URL")
echo "  version: ${VERSION}"
echo "  hash: ${HASH}"
echo "  url: ${PACKAGE_URL}"

echo "Updating default.nix..."
sed -i -E "
    s|(version = \").*(\";)|\1${VERSION}\2|
    s|(url = \").*(\";)|\1${PACKAGE_URL}\2|
    s|(hash = \").*(\";)|\1${HASH}\2|" \
    default.nix

echo "Done"