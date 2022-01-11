#!/bin/sh -e
#
# Install helmfile packages.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../dotfiles/lib/utils.sh"
. "../../dotfiles/lib/buildenv.sh"
. "../../dotfiles/lib/buildenv_asdf.sh"

HELMFILE_VERSION=0.143.0

_preflight() {
    if ! command -v asdf >/dev/null; then
        printe_info "asdf is not installed, skipping helmfile..."
        return 1
    fi
}

_run() {
    printe_h2 "Installing helmfile..."
    _install_helmfile
}

_install_helmfile() {
    asdf_plugin helmfile https://github.com/feniix/asdf-helmfile
    asdf_install helmfile "$HELMFILE_VERSION" global
}

run_with_flavors "$@"
