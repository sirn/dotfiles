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
skalibs_ver=2.8.0.1

printe_h2 "Installing execline..."
require_gmake "execline"


## Setup skalibs
##

if file_absent "$HOME/.local/lib/skalibs"; then
    fetch_gh_archive - skarnet/skalibs "v$skalibs_ver" | tar -C "$build_dir" -xzf -
    cd "$build_dir/skalibs-$skalibs_ver" || exit 1
    ./configure \
        --disable-shared \
        --prefix="$HOME/.local" \
        --libdir="$HOME/.local/lib"
    gmake install
fi


## Setup execline
##

if file_absent "$HOME/.local/bin/execlineb"; then
    fetch_gh_archive - skarnet/execline "v$execline_ver" | tar -C "$build_dir" -xzf -
    cd "$build_dir/execline-$execline_ver" || exit 1
    ./configure \
        --disable-shared \
        --prefix="$HOME/.local" \
        --with-include="$HOME/.local/include" \
        --with-lib="$HOME/.local/lib" \
        --with-sysdeps="$HOME/.local/lib/skalibs/sysdeps"
    gmake install
fi
