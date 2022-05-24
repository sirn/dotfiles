#!/bin/sh -e
#
# Install argocd package.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../dotfiles/lib/utils.sh"
. "../../dotfiles/lib/buildenv.sh"
. "../../dotfiles/lib/buildenv_asdf.sh"

ARGOCD_VERSION=2.3.3

_preflight() {
    if ! command -v asdf >/dev/null; then
        printe_info "asdf is not installed, skipping kustomize..."
        return 1
    fi
}

_run() {
    _install_argocd
}

_install_argocd() {
    asdf_plugin argocd https://github.com/beardix/asdf-argocd
    asdf_install argocd "$ARGOCD_VERSION" global
}

run_with_flavors "$@"
