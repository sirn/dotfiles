#!/bin/sh -e
#
# Install asdf.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../../../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../dotfiles/lib/utils.sh"
. "../../dotfiles/lib/buildenv.sh"
. "../../dotfiles/lib/buildenv_asdf.sh"

_run() {
    printe_h2 "Installing asdf..."
    git_clone https://github.com/asdf-vm/asdf.git "$ASDF_DIR" v0.8.0
}

run_with_flavors "$@"
