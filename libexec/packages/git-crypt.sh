#!/bin/sh -e
#
# Install git-crypt.
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

git_crypt_ver=0.6.0

printe_h2 "Installing git-crypt..."


## Setup
##

if file_absent "$HOME/.local/bin/git-crypt"; then
    fetch_gh_archive - AGWA/git-crypt "$git_crypt_ver" | tar -C "$build_dir" -xzf -
    cd "$build_dir/git-crypt-${git_crypt_ver}" || exit 1
    make
    make install PREFIX="$HOME/.local"
fi
