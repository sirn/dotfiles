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


## Custom setup
##

# Restoring Google Cloud state directory
run_root mkdir -p /usr/local/google-cloud-sdk/.install

# Packages not available under OpenBSD Ports
"../packages/execline.sh"
"../packages/git-crypt.sh"
"../packages/leiningen.sh"


## Hand-off
##

"$base_dir/pkg_asdf.sh" "$flavors"
"$base_dir/pkg_local.sh" "$flavors"
