#!/bin/sh -e
#
# Install Void Linux packages with XBPS.
# See also https://github.com/sirn/void-packages/tree/riscv64 :-)
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "lib/utils.sh"
. "lib/utils_void.sh"

_run() {
    printe_h2 "Installing packages..."

    xbps_install oksh
    xbps_install rsync
    xbps_install sqlite
    xbps_install xtools
    xbps_install xz
}

_run_system() {
    :
}

_run_desktop() {
    :
}

_run_dev() {
    printe_h2 "Installing dev packages..."

    xbps_install base-devel
    xbps_install bzip2-devel
    xbps_install libffi-devel
    xbps_install openssl-devel
    xbps_install readline-devel
    xbps_install sqlite-devel
    xbps_install zlib-devel
}

_run_all() {
    :
}
