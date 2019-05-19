#!/bin/sh -e
#
# Install OpenBSD packages with Pkg.
#

root_dir=${BOOTSTRAP_ROOT:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}
lookup_dir=${LOOKUP_ROOT:-$root_dir}
flavors=$*

# shellcheck source=../../share/bootstrap/funcs.sh
. "$root_dir/share/bootstrap/funcs.sh"

if [ "$(uname)" != "OpenBSD" ]; then
    printe_err "Not an OpenBSD system"
    exit 1
fi


## Installs
##

pkglist=$lookup_dir/var/bootstrap/openbsd/pkglist.txt

for f in $(mangle_file "$pkglist" none "$flavors"); do
    printe_h2 "Installing packages from $f..."
    run_root xargs pkg_add -D snap < "$f"
done

# Only install emacs--no_x11 when other variant of Emacs hasn't been
# installed (e.g. desktop flavor installs emacs--gtk3)
if ! pkg_info -q -e "emacs-*"; then
    run_root pkg_add -D snap emacs--no_x11
fi

# Only install w3m-- when other variant of w3m hasn't been installed
# (e.g. desktop flavor installs w3m--image which is required by emacs-w3m
# when running under GUI mode)
if ! pkg_info -q -e "w3m-*"; then
    run_root pkg_add -D snap w3m--
fi

if [ "$root_dir" = "$lookup_dir" ]; then
    # Restore Google Cloud state directory. This is required for kubectl
    # to be able to authenticate with Google Cloud.
    run_root mkdir -p /usr/local/google-cloud-sdk/.install

    # Packages not available under OpenBSD Ports
    "$root_dir/libexec/packages/execline.sh"
    "$root_dir/libexec/packages/git-crypt.sh"
    "$root_dir/libexec/packages/leiningen.sh"
fi


## Hand-off
##

if [ "$root_dir" = "$lookup_dir" ]; then
    "$root_dir/libexec/bootstrap/pkg_asdf.sh" "$flavors"
    "$root_dir/libexec/bootstrap/pkg_local.sh" "$flavors"
fi
