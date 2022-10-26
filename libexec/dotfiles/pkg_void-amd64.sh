#!/bin/sh -e
#
# Install Void Linux packages with XBPS.
#

BASE_DIR=${BASE_DIR:-$(
    cd "$(dirname "$0")/../.." || exit
    pwd -P
)}

cd "$(dirname "$0")" || exit 1

# shellcheck disable=SC1091
. "lib/utils.sh"

# shellcheck disable=SC1091
. "lib/utils_void.sh"

_run() {
    xbps_install mosh
    xbps_install oksh
    xbps_install xtools
}

_run_system() {
    xbps_install iptables-nft
    xbps_alternative iptables iptables-nft
}

_run_desktop() {
    sh "$BASE_DIR/libexec/packages/sys/fonts.sh" "$@"
}
