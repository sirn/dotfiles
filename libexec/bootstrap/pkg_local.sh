#!/bin/sh -e
#
# Install a package manually and locally.
#

BOOTSTRAP_ROOT=${BOOTSTRAP_ROOT:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}
LOOKUP_ROOT=${LOOKUP_ROOT:-$BOOTSTRAP_ROOT}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$BOOTSTRAP_ROOT/share/bootstrap/funcs.sh"

ensure_paths required same_root

FLAVORS=$*
PLATFORM=$(uname | tr '[:upper:]' '[:lower:]')


## Installs
##

_run_dev() {
    "$BOOTSTRAP_ROOT/libexec/packages/erlang.sh"
    "$BOOTSTRAP_ROOT/libexec/packages/rust.sh"
    "$BOOTSTRAP_ROOT/libexec/packages/node.sh"
    "$BOOTSTRAP_ROOT/libexec/packages/haskell.sh"
}

_run_kubernetes() {
    case $PLATFORM in
        openbsd )
            "$BOOTSTRAP_ROOT/libexec/packages/kubernetes-cli.sh"
            "$BOOTSTRAP_ROOT/libexec/packages/kubernetes-helm.sh"
            ;;

        * )
            ;;
    esac

    "$BOOTSTRAP_ROOT/libexec/packages/kubectx.sh"
    "$BOOTSTRAP_ROOT/libexec/packages/kapitan.sh"
}

run_with_flavors "$FLAVORS"
