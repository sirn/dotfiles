#!/bin/sh -e
#
# Install terraform packages.
#

BASE_DIR=${BASE_DIR:-$(
    cd "$(dirname "$0")/../../.." || exit
    pwd -P
)}

cd "$(dirname "$0")" || exit 1
. "../../dotfiles/lib/utils.sh"
. "../../dotfiles/lib/buildenv.sh"
. "../../dotfiles/lib/buildenv_asdf.sh"

TERRAFORM_VERSION=1.3.3

_preflight() {
    if ! command -v asdf >/dev/null; then
        printe_info "asdf is not installed, skipping terraform..."
        return 1
    fi
}

_run() {
    _install_terraform
}

_install_terraform() {
    asdf_plugin terraform https://github.com/asdf-community/asdf-hashicorp
    asdf_install terraform "$TERRAFORM_VERSION" global
}

run_with_flavors "$@"
