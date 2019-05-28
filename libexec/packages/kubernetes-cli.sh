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

# Using 1.16 since we required an updated vendor:
# See https://github.com/kubernetes/kubernetes/tree/master/vendor
kubectl_ver=1.16.0-alpha.0
kubectl_sha256=cc8c1214d311d9b78b3e6376d4cff6ad6c98df10df31ad887464623ec0684b53

printe_h2 "Installing kubectl..."
require_bin go

GOPATH="$build_dir/go"; export GOPATH
PATH="$GOPATH/bin:$PATH"


## Setup
##

if is_force || file_absent "$HOME/.local/bin/kubectl"; then
    cd "$build_dir" || exit 1

    fetch_gh_archive kubectl.tar.gz kubernetes/kubernetes "v$kubectl_ver"
    verify_shasum kubectl.tar.gz $kubectl_sha256
    tar -C "$build_dir" -xzf kubectl.tar.gz
    rm kubectl.tar.gz

    mkdir -p "$build_dir/go/src/k8s.io"
    mv "$build_dir/kubernetes-$kubectl_ver" "$build_dir/go/src/k8s.io/kubernetes"

    cd "$build_dir/go/src/k8s.io/kubernetes/cmd/kubectl" || exit 1
    go install .
    cp "$build_dir/go/bin/kubectl" "$HOME/.local/bin/kubectl"
    printe "kubectl has been successfully installed"
fi
