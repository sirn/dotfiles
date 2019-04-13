#!/bin/sh -e
#
# Install FreeBSD packages with Pkgng.
#

root_dir=${BOOTSTRAP_ROOT:-../../}
lookup_dir=${LOOKUP_ROOT:-$root_dir}
flavors=$*

# shellcheck source=../../share/bootstrap/funcs.sh
. "$root_dir/share/bootstrap/funcs.sh"

if [ "$(uname)" != "FreeBSD" ]; then
    printe_err "Not a FreeBSD system"
    exit 1
fi


## Setup
##

if [ ! -x /usr/local/sbin/pkg ]; then
    printe_h2 "Bootstrapping pkgng..."
    run_root ASSUME_ALWAYS_YES=yes pkg bootstrap
fi


## Installs
##

pkglist=$lookup_dir/var/bootstrap/freebsd/pkglist.txt

for f in $(mangle_file "$pkglist" none "$flavors"); do
    printe_h2 "Installing packages from ${f##$lookup_dir/}..."
    run_root xargs pkg install -y < "$f"
done


## Hand-off
##

"$root_dir/libexec/bootstrap/pkg_asdf.sh" "$flavors"
"$root_dir/libexec/bootstrap/pkg_local.sh" "$flavors"
