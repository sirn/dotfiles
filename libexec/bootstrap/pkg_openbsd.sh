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


## Compatibility Shims
##
## Build-in programs in OpenBSD such as tar or diff is strictly POSIX and doesn't
## support many features many tools have come to expect, so we need to default
## some of those tools to the GNU (ew) ones...
##

_make_link() {
    src=$1; shift
    dest=$1; shift

    if [ ! -f "$src" ]; then
        printe "$src doesn't exists, skipping..."
        return
    fi

    if [ -f "$dest" ] || [ ! -L "$dest" ]; then
        printe "$dest already exists and is not a link, skipping"
        return
    fi

    if [ "$(readlink "$dest")" = "$src" ]; then
        printe "$dest already linked"
        return
    fi

    mkdir -p "$(dirname "$dest")"
    rm -f "$dest"
    ln -s "$src" "$dest"
    printe "$dest has been linked to $src"
}

printe_h2 "Linking compability shims..."

_make_link /usr/local/bin/gtar "$HOME/.local/bin/tar"
_make_link /usr/local/bin/gdiff "$HOME/.local/bin/diff"
_make_link /usr/local/bin/gpatch "$HOME/.local/bin/patch"


## Hand-off
##

"$base_dir/pkg_asdf.sh" "$flavors"
"$base_dir/pkg_local.sh" "$flavors"
