#!/bin/sh -e
#
# Install a package manually and locally.
#

base_dir=$(cd "$(dirname "$0")/" || exit; pwd -P)
platform=$(uname | tr '[:upper:]' '[:lower:]')
flavors=$*

cd "$base_dir" || exit 1
. ../../share/bootstrap/funcs.sh


## Build scripts
##

"../packages/rust.sh"
"../packages/node.sh"
"../packages/haskell.sh"


## Kubernetes flavor
##

if has_args "kubernetes" "$flavors"; then
    case $platform in
        openbsd )
            "../packages/kubernetes-cli.sh"
            "../packages/kubernetes-helm.sh"
            ;;

        * )
            ;;
    esac

    "../packages/kubectx.sh"
    "../packages/kapitan.sh"
fi
