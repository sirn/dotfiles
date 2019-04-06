#!/bin/sh -e
#
# Install FreeBSD packages with Pkgng.
#

base_dir=$(cd "$(dirname "$0")/" || exit; pwd -P)
flavors=$*

cd "$base_dir" || exit 1
. ../../share/bootstrap/funcs.sh
. ../../share/bootstrap/compat.sh

if [ "$(uname)" != "FreeBSD" ]; then
    printe_err "Not a FreeBSD system"
    exit 1
fi


## Utils
##

_install_pkg() {
    pkglist=$1; shift

    if [ ! -f "$pkglist" ]; then
        printe_info "${pkglist##../../} could not be found, skipping"
        return
    fi

    printe_h2 "Installing packages from ${pkglist##../../}..."
    run_root xargs pkg install -y < "$pkglist"
}


## Setup
##

if [ ! -x /usr/local/sbin/pkg ]; then
    printe_h2 "Bootstrapping pkgng..."
    run_root ASSUME_ALWAYS_YES=yes pkg bootstrap
fi


## Installs
##

_install_pkg "../../var/bootstrap/freebsd/pkglist.txt"

for flavor in $flavors; do
    _install_pkg "../../var/bootstrap/freebsd/pkglist.${flavor}.txt"
done


## Hand-off
##

"$base_dir/pkg_asdf.sh" "$flavors"
"$base_dir/pkg_local.sh" "$flavors"
