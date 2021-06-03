#!/bin/sh -e
#
# Configure current user on Void Linux (WSL variant).
# See also https://github.com/sirn/void-packages/tree/riscv64 :-)
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "./user_void-amd64.sh"

_run() {
    _setup_user_links
    _setup_user_shell
}

_run_desktop() {
    :
}

_run_dev() {
    :
}
