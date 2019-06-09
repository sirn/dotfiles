#!/bin/sh -e
#
# Install kubectx.
#

BOOTSTRAP_ROOT=${BOOTSTRAP_ROOT:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$BOOTSTRAP_ROOT/share/bootstrap/funcs.sh"

FLAVORS=$*
KUBECTX_VERSION=0.6.3


## Run
##

_run_kubernetes() {
    printe_h2 "Installing kubectx..."

    git_clone https://github.com/ahmetb/kubectx.git "$HOME/.local/src/kubectx" v$KUBECTX_VERSION
    make_link "$HOME/.local/src/kubectx/kubectx" "$HOME/.local/bin/kubectx"
    make_link "$HOME/.local/src/kubectx/kubens" "$HOME/.local/bin/kubens"
}

run_with_flavors "$FLAVORS"
