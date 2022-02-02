#!/bin/sh -e
#
# Shared functions for Darwin
#

MACPORTS=/opt/local/bin/port
MACPORTS_VER=2.7.1
MACPORTS_SHA256=fad5a11874e65dfe3fb3688f98e8ae2c90a154b89ce04888a4f5a633509648b3

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
    variants=$*
    active=$($MACPORTS -q installed "$pkg" 2>&1 | grep "(active)")

    # Not installed
    if [ -z "$active" ]; then
        return 1
    fi

    # Installed, but not the requested variant
    for v in $variants; do
        if echo "$active" | grep -qv "$v"; then
            return 1
        fi
    done

    return 0
}

macports_install() {
    pkg=$1; shift

    if macports_installed "$pkg" "$@"; then
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

install_launchd() {
    OPTIND=1

    plist_src=
    plist_dest=
    plist_load=

    while getopts "Sd:l" opt; do
        case "$opt" in
            d ) plist_dir="$OPTARG";;
            l ) plist_load=1;;
            * )
                printe_err "Invalid flags given to install_svc"
                exit 1
                ;;
        esac
    done

    shift $((OPTIND-1))

    if [ "${1:-}" = "--" ]; then
        shift
    fi

    if [ -z "$plist_dir" ]; then
        plist_dir="$HOME/Library/LaunchAgents"
    fi

    plist_src=$1; shift
    plist_filename=$(basename "$plist_src")
    plist_dest="$plist_dir/$plist_filename"

    if [ -f "$plist_dest" ]; then
        if diff "$plist_src" "$plist_dest" >/dev/null; then
            printe_info "$plist_dest"
            return
        fi

        if [ "$plist_load" = "1" ]; then
            launchctl unload -w "$plist_dest" 2>/dev/null
        fi
    fi

    if [ ! -d "$plist_dir" ]; then
        mkdir -p "$plist_dir"
    fi

    cp "$plist_src" "$plist_dest"
    chmod 0644 "$plist_dest"

    if [ "$plist_load" = "1" ]; then
        launchctl load -w "$plist_dest"
    fi

    printe_info "$plist_dest has been installed"
}
