#!/bin/sh -e
#
# Configure current user shell.
#

BOOTSTRAP_ROOT=${BOOTSTRAP_ROOT:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}
LOOKUP_ROOT=${LOOKUP_ROOT:-$BOOTSTRAP_ROOT}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$BOOTSTRAP_ROOT/share/bootstrap/funcs.sh"

ensure_paths required same_root

PLATFORM=$(uname | tr '[:upper:]' '[:lower:]')
FLAVORS=$*
CONF_FILE=$(mangle_file1 "$BOOTSTRAP_ROOT/var/bootstrap/shell.txt" "$PLATFORM")


## Runs
##

_run() {
    if [ -z "$CONF_FILE" ]; then
        printe_info "Shell configuration not found, skipping"
        exit
    fi

    printe_h2 "Setting current user shell from $CONF_FILE..."

    target_shell="$(cat "$CONF_FILE")"

    if [ -z "$target_shell" ]; then
        printe "Shell configuration seems to be empty, skipping"
        exit
    fi

    if ! target_shell_bin=$(command -v "$target_shell"); then
        printe_err "$target_shell is not a valid shell, aborting"
        exit 1
    fi

    if ! grep -q "$target_shell_bin" /etc/shells; then
        printe_info "Adding $target_shell_bin to /etc/shells..."
        printf "%s\\n" "$target_shell_bin" | run_root tee -a /etc/shells
    fi

    run_root chsh -s "$target_shell_bin" "$USER"
}

run_with_flavors "$FLAVORS"
