#!/bin/sh -e
#
# Install a package manually and locally.
#

root_dir=${BOOTSTRAP_ROOT:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}
lookup_dir=${LOOKUP_ROOT:-$root_dir}
flavors=$*

platform=$(uname | tr '[:upper:]' '[:lower:]')

# shellcheck source=../../share/bootstrap/funcs.sh
. "$root_dir/share/bootstrap/funcs.sh"

if [ "$root_dir" != "$lookup_dir" ]; then
    printe_err "Cannot be included from different root"
    exit 1
fi


## Build scripts
##

"$root_dir/libexec/packages/erlang.sh"
"$root_dir/libexec/packages/rust.sh"
"$root_dir/libexec/packages/node.sh"
"$root_dir/libexec/packages/haskell.sh"


## Kubernetes flavor
##

if has_args "kubernetes" "$flavors"; then
    case $platform in
        openbsd )
            "$root_dir/libexec/packages/kubernetes-cli.sh"
            "$root_dir/libexec/packages/kubernetes-helm.sh"
            ;;

        * )
            ;;
    esac

    "$root_dir/libexec/packages/kubectx.sh"
    "$root_dir/libexec/packages/kapitan.sh"
fi
