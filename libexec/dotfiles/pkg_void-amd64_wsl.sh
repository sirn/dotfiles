#!/bin/sh -e
#
# Install Void Linux packages with XBPS.
#
#shellcheck disable=SC1091

BASE_DIR=${BASE_DIR:-$(
    cd "$(dirname "$0")/../.." || exit
    pwd -P
)}

cd "$BASE_DIR" || exit 1

. "$BASE_DIR/libexec/dotfiles/lib/utils.sh"
. "$BASE_DIR/libexec/dotfiles/lib/utils_void.sh"

_run() {
    xbps_install mosh
    xbps_install opendoas
    xbps_install xtools
}
