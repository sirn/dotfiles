#!/bin/sh -e
#
# Install terragrunt packages.
#

BASE_DIR=${BASE_DIR:-$(
    cd "$(dirname "$0")/../../.." || exit
    pwd -P
)}

cd "$(dirname "$0")" || exit 1
. "../../dotfiles/lib/utils.sh"
. "../../dotfiles/lib/buildenv.sh"
. "../../dotfiles/lib/buildenv_asdf.sh"

TERRAGRUNT_VERSION=0.38.5

_preflight() {
    if ! command -v asdf >/dev/null; then
        printe_info "asdf is not installed, skipping terragrunt..."
        return 1
    fi
}

_run() {
    _install_terragrunt
}

_install_terragrunt() {
    asdf_plugin terragrunt https://github.com/ohmer/asdf-terragrunt
    asdf_install terragrunt "$TERRAGRUNT_VERSION" global
}

run_with_flavors "$@"
