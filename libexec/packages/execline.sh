#!/bin/sh -e
#
# Install execline.
#

root_dir=${BOOTSTRAP_ROOT:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$root_dir/share/bootstrap/funcs.sh"


## Tmp
##

build_dir=$(mktemp -d)
if ! normalize_bool "$NO_CLEAN_BUILDDIR"; then
    trap 'rm -rf $build_dir' 0 1 2 3 6 14 15
fi


## Preparation
##

execline_ver=2.5.1.0
execline_sha256=965a915ebf158e221b1c56078a54dfa55b09b9b51a0a25edc7013ae1e12e3f33

skalibs_ver=2.8.0.1
skalibs_sha256=88a6000634cf8477b8649604984534fee11997ac0c08a271881a4974e30968f5

printe_h2 "Installing execline..."

case $(uname) in
    FreeBSD | OpenBSD )
        require_bin gmake
        ;;
esac


## Setup skalibs
##

if is_force || file_absent "$HOME/.local/lib/skalibs"; then
    cd "$build_dir" || exit 1

    fetch_gh_archive skalibs.tar.gz skarnet/skalibs "v$skalibs_ver"
    verify_shasum skalibs.tar.gz $skalibs_sha256
    tar -C "$build_dir" -xzf skalibs.tar.gz
    rm skalibs.tar.gz

    cd "$build_dir/skalibs-$skalibs_ver" || exit 1
    ./configure \
        --disable-shared \
        --prefix="$HOME/.local" \
        --libdir="$HOME/.local/lib"
    gmake install
fi


## Setup execline
##

if is_force || file_absent "$HOME/.local/bin/execlineb"; then
    cd "$build_dir" || exit 1

    fetch_gh_archive execline.tar.gz skarnet/execline "v$execline_ver"
    verify_shasum execline.tar.gz $execline_sha256
    tar -C "$build_dir" -xzf execline.tar.gz
    rm execline.tar.gz

    cd "$build_dir/execline-$execline_ver" || exit 1
    ./configure \
        --disable-shared \
        --prefix="$HOME/.local" \
        --with-include="$HOME/.local/include" \
        --with-lib="$HOME/.local/lib" \
        --with-sysdeps="$HOME/.local/lib/skalibs/sysdeps"
    gmake install
fi
