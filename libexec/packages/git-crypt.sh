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
git_crypt_sha256=777c0c7aadbbc758b69aff1339ca61697011ef7b92f1d1ee9518a8ee7702bb78

printe_h2 "Installing git-crypt..."


## Setup
##

if is_force || file_absent "$HOME/.local/bin/git-crypt"; then
    cd "$build_dir" || exit 1

    fetch_gh_archive git-crypt.tar.gz AGWA/git-crypt "$git_crypt_ver"
    verify_shasum git-crypt.tar.gz $git_crypt_sha256
    tar -C "$build_dir" -xzf git-crypt.tar.gz
    rm git-crypt.tar.gz

    cd "$build_dir/git-crypt-${git_crypt_ver}" || exit 1
    make
    make install PREFIX="$HOME/.local"
fi
