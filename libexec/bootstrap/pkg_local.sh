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
    "$BOOTSTRAP_ROOT/libexec/packages/erlang.sh" "$FLAVORS"
    "$BOOTSTRAP_ROOT/libexec/packages/rust.sh" "$FLAVORS"
    "$BOOTSTRAP_ROOT/libexec/packages/node.sh" "$FLAVORS"
    "$BOOTSTRAP_ROOT/libexec/packages/haskell.sh" "$FLAVORS"
}

_run_kubernetes() {
    case $PLATFORM in
        openbsd )
            "$BOOTSTRAP_ROOT/libexec/packages/kubernetes-cli.sh" "$FLAVORS"
            "$BOOTSTRAP_ROOT/libexec/packages/kubernetes-helm.sh" "$FLAVORS"
            ;;

        * )
            ;;
    esac

    "$BOOTSTRAP_ROOT/libexec/packages/kubectx.sh" "$FLAVORS"
    "$BOOTSTRAP_ROOT/libexec/packages/kapitan.sh" "$FLAVORS"
}

run_with_flavors "$FLAVORS"
