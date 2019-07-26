#!/bin/sh -e
#
# Shared functions for Darwin
#

PLATFORM_VERS=$(sw_vers -productVersion)

MAS=/opt/local/bin/mas
MAS_PLATFORM=10.14

MACPORTS=/opt/local/bin/port
MACPORTS_VER=2.5.4
MACPORTS_SHA256=592e4a021588f37348fe7b806c202e4d77f75bcff1a0b20502d5f1177c2c21ff

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

mas_bootstrap() {
    if [ ! -x $MACPORTS ]; then
        printe_err "MacPorts is required to bootstrap MAS"
        exit 1
    fi

    if version_gte "$MAS_PLATFORM" "$PLATFORM_VERS"; then
        if [ ! -x $MAS ]; then
            printe_h2 "Bootstrapping mas..."
            run_root $MACPORTS -Nf install mas
        fi
    else
        printe_info "mas command line utility requires macOS <= $MAS_PLATFORM"
    fi
}

mas_install() {
    pkg_id=$1; shift
    app_name=$*

    if [ -d "/Applications/$app_name.app" ]; then
        printe "$app_name (mas) already installed"
        return
    fi

    if ! version_gte "$MAS_PLATFORM" "$PLATFORM_VERS"; then
        printe "mas is not available, please manually install $app_name"
        return
    fi

    printe_h2 "Installing $app_name (mas)..."

    if ! "$MAS" install "$pkg_id"; then
        printe_info "$app_name (mas) failed to install, skipping..."
    fi
}
