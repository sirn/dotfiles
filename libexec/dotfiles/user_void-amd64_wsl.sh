#!/bin/sh -e
#
# Configure current user on Void Linux.
#
#shellcheck disable=SC1091

BASE_DIR=${BASE_DIR:-$(
    cd "$(dirname "$0")/../.." || exit
    pwd -P
)}

cd "$BASE_DIR" || exit 1

. "$BASE_DIR/libexec/dotfiles/lib/utils.sh"
. "$BASE_DIR/libexec/dotfiles/lib/utils_void.sh"
. "$BASE_DIR/libexec/dotfiles/lib/utils_runit.sh"

_run() {
    update_shells oksh
    change_shell oksh
}
