#!/bin/sh -e
#
# Configure current user on Void Linux (WSL variant).
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "./user_void.sh"

_run() {
    _setup_user_links
    _setup_user_shell
}
