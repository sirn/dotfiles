#!/bin/sh -e
#
# Install Darwin packages with MacPorts and MAS.
#
#shellcheck disable=SC1091

BASE_DIR=${BASE_DIR:-$(
    cd "$(dirname "$0")/../.." || exit
    pwd -P
)}

cd "$BASE_DIR" || exit 1

. "$BASE_DIR/libexec/dotfiles/lib/utils.sh"
. "$BASE_DIR/libexec/dotfiles/lib/utils_darwin.sh"

_preflight() {
    if ! command -v port >/dev/null; then
        printe_err "MacPorts is not installed, not continuing with packages"
        printe_info "See installation instructions at: https://www.macports.org/install.php"
        exit 1
    fi
}

_run() {
    macports_install carthage
    macports_install mosh
    macports_install oksh
    macports_install opendoas
    macports_install xcodes
}

_run_desktop() {
    macports_install emacs-mac-app -nativecomp
}

_run_system() {
    macports_install macfuse
}
