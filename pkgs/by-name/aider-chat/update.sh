#!/usr/bin/env nix-shell
#!nix-shell -i bash -p curl -p jq -p nix-prefetch-github

BASE_DIR=$(
    cd "$(dirname "$0")" || exit
    pwd -P
)

## Prelude
## ----------------------------------------------------------------------------

cd "${BASE_DIR}" || exit 1

OWNER="Aider-AI"
REPO="aider"

## Get latest version
## ----------------------------------------------------------------------------

echo "Fetching latest version..."
LATEST_TAG=$(curl -s "https://api.github.com/repos/${OWNER}/${REPO}/releases" | jq -r '.[0].tag_name')
VERSION=${LATEST_TAG#v}
echo "  Latest version: ${VERSION}"

## Nix
## ----------------------------------------------------------------------------

echo "Fetching hash..."
HASH=$(nix-prefetch-github $OWNER $REPO --rev "v${VERSION}" | jq -r '.hash')
echo "  hash: ${HASH}"

echo "Updating default.nix..."
sed -i -E "
    s|(version =).*|\1 \"${VERSION}\";|
    s|(hash = ).*|\1\"${HASH}\";|" \
    default.nix


echo "Done"
