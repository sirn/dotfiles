#!/bin/sh -e
#
# Configure current user on Darwin.
#

BASE_DIR=${BASE_DIR:-$(
    cd "$(dirname "$0")/../.." || exit
    pwd -P
)}

cd "$(dirname "$0")" || exit 1

# shellcheck disable=SC1091
. "lib/utils.sh"

# shellcheck disable=SC1091
. "lib/utils_darwin.sh"

_run() {
    _setup_user_shell
}

_setup_user_shell() {
    change_shell oksh
}
