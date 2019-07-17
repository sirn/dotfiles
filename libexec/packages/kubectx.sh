#!/bin/sh -e
#
# Install kubectx.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../share/bootstrap/utils.sh"

KUBECTX_VERSION=0.6.3

_run() {
    printe_h2 "Installing kubectx..."

    git_clone \
        https://github.com/ahmetb/kubectx.git \
        "$HOME/.local/src/kubectx" \
        v$KUBECTX_VERSION

    make_link "$HOME/.local/src/kubectx/kubectx" "$HOME/.local/bin/kubectx"
    make_link "$HOME/.local/src/kubectx/kubens" "$HOME/.local/bin/kubens"
}

_run
