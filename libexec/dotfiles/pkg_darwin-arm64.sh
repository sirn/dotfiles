#!/bin/sh -e
#
# Install Darwin packages with MacPorts and MAS.
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
    macports_bootstrap
    macports_install carthage
    macports_install mosh
    macports_install oksh
    macports_install xcodes
}

_run_desktop() {
    macports_install emacs-mac-app
}
