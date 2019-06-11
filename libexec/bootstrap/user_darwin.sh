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
IPFS_PLIST=$HOME/Library/LaunchAgents/io.ipfs.ipfs.plist


## Setup
##

_setup_ipfs() {
    printe_h2 "Setting up ipfs..."

    if ! command -v ipfs >/dev/null; then
        printe_info "ipfs is not installed, skipping..."
        return
    fi

    if is_force || [ ! -f "$IPFS_PLIST" ]; then
        cp "$BOOTSTRAP_ROOT/share/examples/launchd/io.ipfs.ipfs.plist" "$IPFS_PLIST"
        chmod 0644 "$IPFS_PLIST"
        launchctl load -w "$IPFS_PLIST"
        printe "$IPFS_PLIST has been installed, you may need to relogin"
    else
        printe "$IPFS_PLIST already exists"
    fi
}


## Run
##

_run() {
    _setup_ipfs

    "$BOOTSTRAP_ROOT/libexec/bootstrap/user_shell.sh" "$FLAVORS"
    "$BOOTSTRAP_ROOT/libexec/bootstrap/user_links.sh" "$FLAVORS"
}

run_with_flavors "$FLAVORS"
