#!/bin/sh -e
#
# Install git-crypt.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../share/bootstrap/utils.sh"
. "../../share/bootstrap/buildenv.sh"

GITCRYPT_VER=0.6.0
GITCRYPT_SHA256=777c0c7aadbbc758b69aff1339ca61697011ef7b92f1d1ee9518a8ee7702bb78

_run() {
    printe_h2 "Installing git-crypt..."

    if ! forced && [ -f "$HOME/.local/bin/git-crypt" ]; then
        printe_info "$HOME/.local/bin/git-crypt already exists, skipping..."
        return
    fi

    cd "$BUILD_DIR" || exit 1

    fetch_gh_archive git-crypt.tar.gz AGWA/git-crypt "$GITCRYPT_VER"
    verify_shasum git-crypt.tar.gz $GITCRYPT_SHA256
    tar -C "$BUILD_DIR" -xzf git-crypt.tar.gz
    rm git-crypt.tar.gz

    cd "$BUILD_DIR/git-crypt-${GITCRYPT_VER}" || exit 1
    make
    make install PREFIX="$HOME/.local"
}

_run
