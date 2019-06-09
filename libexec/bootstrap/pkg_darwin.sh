#!/bin/sh -e
#
# Install Darwin packages with MacPorts and MAS.
#

BOOTSTRAP_ROOT=${BOOTSTRAP_ROOT:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}
LOOKUP_ROOT=${LOOKUP_ROOT:-$BOOTSTRAP_ROOT}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$BOOTSTRAP_ROOT/share/bootstrap/funcs.sh"

ensure_paths required
ensure_platform "Darwin"

FLAVORS=$*
BUILD_DIR=$(make_temp)
PLATFORM_VERS=$(sw_vers -productVersion)

MAS=/opt/local/bin/mas
MAS_MAX_PLATFORM=10.14

MACPORTS=/opt/local/bin/port
MACPORTS_VER=2.5.4
MACPORTS_SHA256=592e4a021588f37348fe7b806c202e4d77f75bcff1a0b20502d5f1177c2c21ff

PKGLIST=$LOOKUP_ROOT/var/bootstrap/darwin/pkglist.txt


## Utils
##

_do_macports_install() {
    pkg=$1; shift

    case "$($MACPORTS installed "$pkg" 2>&1)" in
        "None of the"* ) ;;
        * )
            printe "$pkg (macports) already installed"
            return
            ;;
    esac

    printe "Installing $pkg (macports)..."

    if ! run_root $MACPORTS -n install "$pkg"; then
        printe_info "$pkg (macports) failed to install, skipping..."
    fi
}

_do_mas_install() {
    pkg_id=$1; shift
    app_name=$*

    if [ -d "/Applications/$app_name.app" ]; then
        printe "$app_name (mas) already installed"
        return
    fi

    if ! version_gte "$MAS_MAX_PLATFORM" "$PLATFORM_VERS"; then
        printe "mas is not available, please manually install $app_name"
        return
    fi

    printe "Installing $app_name (mas)..."

    if ! mas install "$pkg_id"; then
        printe_info "$app_name (mas) failed to install, skipping..."
    fi
}

_do_exec() {
    path=$1; shift
    "$LOOKUP_ROOT/$path" "$FLAVORS"
}


## Setup
##

_setup_env() {
    if [ ! -x $MACPORTS ]; then
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
    fi

    if version_gte "$MAS_MAX_PLATFORM" "$PLATFORM_VERS"; then
        if [ ! -x $MAS ]; then
            printe_h2 "Bootstrapping mas..."
            run_root $MACPORTS -N install mas
        fi
    else
        printe_info "mas command line utility requires macOS <= 10.14"
    fi
}


## Runs
##

_run() {
    _setup_env

    for f in $(mangle_file "$PKGLIST" none "$FLAVORS"); do
        printe_h2 "Installing packages from $f..."

        while read -r line; do
            case $line in
                "#"* | "" ) continue;;
                *) line=${line%%#*};;
            esac

            eval set -- "$line"

            case "$1" in
                macports ) shift; _do_macports_install "$@";;
                mas )      shift; _do_mas_install "$@";;
                exec )     shift; _do_exec "$@";;
                * ) printe_err "Unknown directive: $1";;
            esac
        done < "$f"
    done
}

run_with_flavors "$FLAVORS"
