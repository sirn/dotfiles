#!/bin/sh -e
#
# Install asdf.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../dotfiles/lib/utils.sh"
. "../../dotfiles/lib/buildenv.sh"
. "../../dotfiles/lib/buildenv_asdf.sh"

_run() {
    git_clone https://github.com/asdf-vm/asdf.git "$ASDF_DIR" v0.9.0
}

run_with_flavors "$@"
