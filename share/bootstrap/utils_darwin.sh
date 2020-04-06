#!/bin/sh -e
#
# Shared functions for Darwin
#

MACPORTS=/opt/local/bin/port
MACPORTS_VER=2.6.2
MACPORTS_SHA256=f4eaf03c9211a9743e1f8134ecd5c307a2979a6f27e2f0700223d390d0157116

macports_bootstrap() {
    if ! forced && [ -f $MACPORTS ]; then
        return
    fi

    printe_h2 "Bootstrapping MacPorts..."

    xcode-select --install 2>/dev/null || true
    cd "$BUILD_DIR" || exit 1

    fetch_gh_archive macports.tar.gz macports/macports-base v$MACPORTS_VER
    verify_shasum macports.tar.gz $MACPORTS_SHA256
    tar -C "$BUILD_DIR" -xzf macports.tar.gz
    rm macports.tar.gz

    cd "$BUILD_DIR/macports-base-$MACPORTS_VER" || exit 1
    ./configure && make
    run_root make install
    run_root $MACPORTS sync
}

macports_install() {
    pkg=$1; shift

    case "$($MACPORTS installed "$pkg" 2>&1)" in
        "None of the"* ) ;;
        * )
            printe "$pkg (macports) already installed"
            return
            ;;
    esac

    printe_h2 "Installing $pkg (macports)..."

    if ! run_root $MACPORTS -n install "$pkg" "$@"; then
        printe_info "$pkg (macports) failed to install, skipping..."
    fi
}

macports_select() {
    sel=$1; shift
    pkg=$1; shift

    printe_info "Selecting $pkg as default version for $sel (macports)..."
    run_root $MACPORTS select "$sel" "$pkg"
}
