#!/bin/sh -e
#
# Configure current user on Darwin.
#
#shellcheck disable=SC1091

BASE_DIR=${BASE_DIR:-$(
    cd "$(dirname "$0")/../.." || exit
    pwd -P
)}

cd "$BASE_DIR" || exit 1

. "$BASE_DIR/libexec/dotfiles/lib/utils.sh"
. "$BASE_DIR/libexec/dotfiles/lib/utils_darwin.sh"
. "$BASE_DIR/libexec/dotfiles/lib/utils_launchd.sh"

_run_system() {
    install_launchd -Sl "$BASE_DIR/etc/launchd/sh.null.maxfiles.plist"
}
