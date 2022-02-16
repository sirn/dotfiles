#!/bin/sh -e
#
# Install nomad packages.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../dotfiles/lib/utils.sh"
. "../../dotfiles/lib/buildenv.sh"
. "../../dotfiles/lib/buildenv_asdf.sh"

NOMAD_VERSION=1.2.6

_preflight() {
    if ! command -v asdf >/dev/null; then
        printe_info "asdf is not installed, skipping nomad..."
        return 1
    fi
}

_run() {
    _install_nomad
}

_install_nomad() {
    asdf_plugin nomad https://github.com/asdf-community/asdf-hashicorp
    asdf_install nomad "$NOMAD_VERSION" global
}

run_with_flavors "$@"
