#!/bin/sh -e
#
# Install git-crypt.
#

BOOTSTRAP_ROOT=${BOOTSTRAP_ROOT:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$BOOTSTRAP_ROOT/share/bootstrap/funcs.sh"

FLAVORS=$*
BUILD_DIR=$(make_temp)

GITCRYPT_VER=0.6.0
GITCRYPT_SHA256=777c0c7aadbbc758b69aff1339ca61697011ef7b92f1d1ee9518a8ee7702bb78


## Run
##

_run_dev() {
    printe_h2 "Installing git-crypt..."

    if is_force || file_absent "$HOME/.local/bin/git-crypt"; then
        cd "$BUILD_DIR" || exit 1

        fetch_gh_archive git-crypt.tar.gz AGWA/git-crypt "$GITCRYPT_VER"
        verify_shasum git-crypt.tar.gz $GITCRYPT_SHA256
        tar -C "$BUILD_DIR" -xzf git-crypt.tar.gz
        rm git-crypt.tar.gz

        cd "$BUILD_DIR/git-crypt-${GITCRYPT_VER}" || exit 1
        make
        make install PREFIX="$HOME/.local"
    fi
}

run_with_flavors "$FLAVORS"
