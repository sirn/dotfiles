#!/bin/sh -e
#
# Install kubectx.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../share/bootstrap/utils.sh"
. "../../share/bootstrap/buildenv.sh"

KUBECTX_VER=0.8.0
KUBECTX_SHA256=7acbb574f2b9cb82c03b2ceaf1d5cf312eddf1cefa12ecf6bc6bf0478511f809

_run() {
    printe_h2 "Installing kubectx..."

    if ! forced && [ -f "$HOME/.local/bin/kubectx" ]; then
        printe_info "$HOME/.local/bin/kubectx already exists, skipping..."
        return
    fi

    cd "$BUILD_DIR" || exit 1

    fetch_gh_archive kubectx.tar.gz ahmetb/kubectx v$KUBECTX_VER
    verify_shasum kubectx.tar.gz $KUBECTX_SHA256
    tar -C "$BUILD_DIR" -xzf kubectx.tar.gz
    rm kubectx.tar.gz

    cd "$BUILD_DIR/kubectx-$KUBECTX_VER" || exit 1
    install -m0755 "kubectx" "$HOME/.local/bin/kubectx"
    install -m0755 "kubens" "$HOME/.local/bin/kubens"
    printe_info "kubectx successfully installed"
}

_run
