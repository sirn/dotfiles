#!/bin/sh -e
#
# Sets up Darwin system.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../share/bootstrap/utils.sh"
. "../../share/bootstrap/utils_darwin.sh"

_run() {
    :
}

run_with_flavors "$@"
