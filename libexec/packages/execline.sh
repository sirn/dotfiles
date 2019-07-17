#!/bin/sh -e
#
# Install execline.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../share/bootstrap/utils.sh"
. "../../share/bootstrap/buildenv.sh"

PLATFORM=$(get_platform)

EXECLINE_VER=2.5.1.0
EXECLINE_SHA256=965a915ebf158e221b1c56078a54dfa55b09b9b51a0a25edc7013ae1e12e3f33
SKALIBS_VER=2.8.0.1
SKALIBS_SHA256=88a6000634cf8477b8649604984534fee11997ac0c08a271881a4974e30968f5

_setup_skalibs() {
    printe_h2 "Installing skalibs..."

    if ! forced && [ -f "$HOME/.local/lib/skalibs" ]; then
        printe_info "$HOME/.local/bin/skalibs already exists, skipping..."
        return
    fi

    case $PLATFORM in
        freebsd | openbsd )
            if ! command -v gmake >/dev/null; then
                printe_info "gmake is required to be installed, skipping.."
                return
            fi
            ;;
    esac

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
}

_setup_execline() {
    printe_h2 "Installing execline..."

    if ! forced && [ -f "$HOME/.local/bin/execlineb" ]; then
        printe_info "$HOME/.local/bin/execlineb already exists, skipping..."
        return
    fi

    case $PLATFORM in
        freebsd | openbsd )
            if ! command -v gmake >/dev/null; then
                printe_info "gmake is required to be installed, skipping.."
                return
            fi
            ;;
    esac

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
}

_run() {
    _setup_skalibs
    _setup_execline
}

_run
