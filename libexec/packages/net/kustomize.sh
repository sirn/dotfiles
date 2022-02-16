#!/bin/sh -e
#
# Install kustomize packages.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../dotfiles/lib/utils.sh"
. "../../dotfiles/lib/buildenv.sh"
. "../../dotfiles/lib/buildenv_asdf.sh"

KUSTOMIZE_VERSION=4.5.2

_preflight() {
    if ! command -v asdf >/dev/null; then
        printe_info "asdf is not installed, skipping kustomize..."
        return 1
    fi
}

_run() {
    _install_kustomize
}

_install_kustomize() {
    asdf_plugin kustomize https://github.com/Banno/asdf-kustomize
    asdf_install kustomize "$KUSTOMIZE_VERSION" global
}

run_with_flavors "$@"
