#!/bin/sh -e
#
# Shared functions for Darwin
#

MACPORTS=/opt/local/bin/port
MACPORTS_VER=2.6.4
MACPORTS_SHA256=535fa1a06fc128280f662a340ee80c7d80a13d823dc6096378b5e8089f469786

macports_bootstrap() {
    if ! forced && [ -f $MACPORTS ]; then
        return
    fi

    printe_h2 "Bootstrapping MacPorts..."

    xcode-select --install 2>/dev/null || true
    cd "$BUILD_DIR" || exit 1

    fetch_gh_archive macports.tar.gz macports/macports-base v$MACPORTS_VER
    verify_shasum macports.tar.gz $MACPORTS_SHA256
    run_tar -C "$BUILD_DIR" -xzf macports.tar.gz
    rm macports.tar.gz

    cd "$BUILD_DIR/macports-base-$MACPORTS_VER" || exit 1

    ./configure && make
    run_root make install
    run_root $MACPORTS sync
}

macports_installed() {
    pkg=$1; shift

    case "$($MACPORTS installed "$pkg" 2>&1)" in
        "None"* )
            return 1
            ;;

        * )
            return 0
            ;;
    esac
}

macports_install() {
    pkg=$1; shift

    if macports_installed "$pkg"; then
        printe "$pkg (macports) already installed"
        return
    fi

    if ! run_root $MACPORTS -N install "$pkg" "$@"; then
        printe_info "$pkg (macports) failed to install, skipping..."
    fi
}

macports_select() {
    sel=$1; shift
    pkg=$1; shift

    case $($MACPORTS select --show "$sel") in
        *"'$sel' is '$pkg'"* )
            printe_info "$pkg is already default version for $sel, skipping..."
            return
            ;;
    esac

    printe_info "Selecting $pkg as default version for $sel (macports)..."
    run_root $MACPORTS select "$sel" "$pkg"
}
