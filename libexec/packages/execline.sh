#!/bin/sh -e
#
# Install execline.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$BASE_DIR/share/bootstrap/funcs.sh"

if [ -z "$BUILD_DIR" ]; then
    BUILD_DIR=$(mktemp -d)
    trap 'rm -rf $BUILD_DIR' 0 1 2 3 6 14 15
fi

EXECLINE_VER=2.5.1.0
EXECLINE_SHA256=965a915ebf158e221b1c56078a54dfa55b09b9b51a0a25edc7013ae1e12e3f33
SKALIBS_VER=2.8.0.1
SKALIBS_SHA256=88a6000634cf8477b8649604984534fee11997ac0c08a271881a4974e30968f5

_run() {
    printe_h2 "Installing execline..."

    case $(uname) in
        FreeBSD | OpenBSD )
            require_bin gmake
            ;;
    esac

    if is_force || file_absent "$HOME/.local/lib/skalibs"; then
        cd "$BUILD_DIR" || exit 1

        fetch_gh_archive skalibs.tar.gz skarnet/skalibs "v$SKALIBS_VER"
        verify_shasum skalibs.tar.gz $SKALIBS_SHA256
        tar -C "$BUILD_DIR" -xzf skalibs.tar.gz
        rm skalibs.tar.gz

        cd "$BUILD_DIR/skalibs-$SKALIBS_VER" || exit 1
        ./configure \
            --disable-shared \
            --prefix="$HOME/.local" \
            --libdir="$HOME/.local/lib"
        gmake install
    fi

    if is_force || file_absent "$HOME/.local/bin/execlineb"; then
        cd "$BUILD_DIR" || exit 1

        fetch_gh_archive execline.tar.gz skarnet/execline "v$EXECLINE_VER"
        verify_shasum execline.tar.gz $EXECLINE_SHA256
        tar -C "$BUILD_DIR" -xzf execline.tar.gz
        rm execline.tar.gz

        cd "$BUILD_DIR/execline-$EXECLINE_VER" || exit 1
        ./configure \
            --disable-shared \
            --prefix="$HOME/.local" \
            --with-include="$HOME/.local/include" \
            --with-lib="$HOME/.local/lib" \
            --with-sysdeps="$HOME/.local/lib/skalibs/sysdeps"
        gmake install
    fi
}

_run
