#!/bin/sh -e
#
# Configure current user on Darwin.
#

BOOTSTRAP_ROOT=${BOOTSTRAP_ROOT:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}
LOOKUP_ROOT=${LOOKUP_ROOT:-$BOOTSTRAP_ROOT}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$BOOTSTRAP_ROOT/share/bootstrap/funcs.sh"

ensure_paths required same_root
ensure_platform "Darwin"

FLAVORS=$*


## Setup
##

_setup_ipfs() {
    printe_h2 "Setting up ipfs..."

    if [ ! -d "$HOME/.ipfs" ]; then
        ipfs init
    fi

    brew services start ipfs
}


## Run
##

_run() {
    _setup_ipfs

    "$BOOTSTRAP_ROOT/libexec/bootstrap/user_shell.sh" "$FLAVORS"
    "$BOOTSTRAP_ROOT/libexec/bootstrap/user_links.sh" "$FLAVORS"
}

_run_desktop() {
    printe_h2 "Setting up chunkwm..."
    brew services start chunkwm

    printe_h2 "Setting up skhd..."
    brew services start skhd
}

run_with_flavors "$FLAVORS"
