#!/bin/sh -e
#
# Install OpenBSD packages with Pkg.
#

base_dir=$(cd "$(dirname "$0")/" || exit; pwd -P)
flavors=$*

cd "$base_dir" || exit 1
. ../../share/bootstrap/funcs.sh
. ../../share/bootstrap/compat.sh

if [ "$(uname)" != "OpenBSD" ]; then
    printe_err "Not an OpenBSD system"
    exit 1
fi


## Installs
##

pkglist="../../var/bootstrap/openbsd/pkglist.txt"

for f in $(mangle_file "$pkglist" none "$flavors"); do
    printe_h2 "Installing packages from ${f##../../}..."
    run_root xargs pkg_add -D snap < "$f"
done


## Hand-off
##

"$base_dir/pkg_asdf.sh" "$flavors"
"$base_dir/pkg_local.sh" "$flavors"
