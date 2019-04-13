#!/bin/sh -e
#
# Install Kubernetes CLI.
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

# Using master until updated vendor is in release:
# See https://github.com/kubernetes/kubernetes/tree/master/vendor
kubectl_ver=master

printe_h2 "Installing kubectl..."
require_bin go

GOPATH="$build_dir/go"; export GOPATH
PATH="$GOPATH/bin:$PATH"


## Setup
##

if is_force || file_absent "$HOME/.local/bin/kubectl"; then
    fetch_gh_archive - kubernetes/kubernetes "$kubectl_ver" | tar -C "$build_dir" -xzf -
    mkdir -p "$build_dir/go/src/k8s.io"
    mv "$build_dir/kubernetes-$kubectl_ver" "$build_dir/go/src/k8s.io/kubernetes"

    cd "$build_dir/go/src/k8s.io/kubernetes/cmd/kubectl" || exit 1
    go install .
    cp "$build_dir/go/bin/kubectl" "$HOME/.local/bin/kubectl"
    printe "kubectl has been successfully installed"
fi
