#!/bin/sh -e
#
# Install OpenBSD packages with Pkg.
#

base_dir=$(cd "$(dirname "$0")/" || exit; pwd -P)
flavors=$*

cd "$base_dir" || exit 1
. ../../share/bootstrap/funcs.sh

if [ "$(uname)" != "OpenBSD" ]; then
    printe_err "Not an OpenBSD system"
    exit 1
fi


## Installs
##

pkglist=../../var/bootstrap/openbsd/pkglist.txt

for f in $(mangle_file $pkglist none "$flavors"); do
    printe_h2 "Installing packages from ${f##../../}..."
    run_root xargs pkg_add -D snap < "$f"
done


## Compatibility Shims
##
## Build-in programs in OpenBSD such as tar or diff is strictly POSIX and doesn't
## support many features many tools have come to expect, so we need to default
## some of those tools to the GNU (ew) ones...
##

printe_h2 "Linking compability shims..."

make_link /usr/local/bin/gtar "$HOME/.local/bin/tar"
make_link /usr/local/bin/gdiff "$HOME/.local/bin/diff"


## Hand-off
##

"$base_dir/pkg_asdf.sh" "$flavors"
"$base_dir/pkg_local.sh" "$flavors"
