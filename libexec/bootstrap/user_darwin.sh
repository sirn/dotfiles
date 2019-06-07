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


## Run
##

_run() {
    "$BOOTSTRAP_ROOT/libexec/bootstrap/user_shell.sh" "$FLAVORS"
    "$BOOTSTRAP_ROOT/libexec/bootstrap/user_links.sh" "$FLAVORS"
}

run_with_flavors "$FLAVORS"
