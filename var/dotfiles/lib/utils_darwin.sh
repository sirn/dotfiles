#!/bin/sh -e
#
# Shared functions for Darwin
#

MACPORTS=/opt/local/bin/port
MACPORTS_VER=2.6.3
MACPORTS_SHA256=99e9eea869bf8d72b8a9c85ae91a3ca3159ac77de685768ffbba8a2335de5dde

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
    OLDIFS=$IFS
    IFS=,

    # shellcheck disable=SC2116
    for pkg in $(echo "$@"); do
        pkgname=$(trim "$pkg")
        pkgflag=${pkgname#* }
        if [ "$pkgflag" = "$pkgname" ]; then
            pkgflag=
        fi

        pkgname=${pkgname%%$pkgflag}
        pkgname=$(trim "$pkgname")
        if macports_installed "$pkgname"; then
            printe "$pkgname (macports) already installed"
            continue
        fi

        if ! run_root $MACPORTS -N install "$pkgname" $pkgflag; then
            printe_info "$pkg (macports) failed to install, skipping..."
        fi
    done

    IFS=$OLDIFS
}

macports_select() {
    sel=$1; shift
    pkg=$1; shift

    printe_info "Selecting $pkg as default version for $sel (macports)..."
    run_root $MACPORTS select "$sel" "$pkg"
}
