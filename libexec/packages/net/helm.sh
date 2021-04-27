#!/bin/sh -e
#
# Install helm packages.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../dotfiles/lib/utils.sh"
. "../../dotfiles/lib/buildenv.sh"
. "../../dotfiles/lib/buildenv_asdf.sh"

HELM_VERSION=3.5.4
HELM_VERSION_PATH=$ASDF_DIR/installs/helm/$HELM_VERSION

_preflight() {
    if ! command -v asdf >/dev/null; then
        printe_info "asdf is not installed, skipping helm..."
        return 1
    fi
}

_run() {
    printe_h2 "Installing helm..."
    _install_helm
}

_install_helm() {
    asdf_plugin helm https://github.com/Antiarchitect/asdf-helm
    asdf_install helm "$HELM_VERSION" global
}

run_with_flavors "$@"
