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
USERENV_PLIST="$HOME/Library/LaunchAgents/th.in.grid.userenv.plist"


## Setup
##

_setup_userenv() {
    printe_h2 "Setting up userenv..."

    if is_force || [ ! -f "$USERENV_PLIST" ]; then
        cp "$BOOTSTRAP_ROOT/share/examples/launchd/th.in.grid.userenv.plist" "$USERENV_PLIST"
        chmod 0644 "$USERENV_PLIST"
        launchctl load -w "$USERENV_PLIST"
        printe "$USERENV_PLIST has been installed, you may need to relogin"
    else
        printe "$USERENV_PLIST already exists"
    fi
}

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
    _setup_userenv
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
