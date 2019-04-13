#!/bin/sh -e
#
# Install Kubernetes Helm.
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

helm_ver=2.13.1

printe_h2 "Installing helm..."
require_go "helm"
require_gmake "kubectl"

GOPATH="$build_dir/go"; export GOPATH
PATH="$GOPATH/bin:$PATH"


## Setup
##

if file_absent "$HOME/.local/bin/helm"; then
    fetch_gh_archive - helm/helm "v$helm_ver" | tar -C "$build_dir" -xzf -
    mkdir -p "$build_dir/go/src/k8s.io"
    mv "$build_dir/helm-$helm_ver" "$build_dir/go/src/k8s.io/helm"

    cd "$build_dir/go/src/k8s.io/helm"
    gmake bootstrap build

    cp "$build_dir/go/src/k8s.io/helm/bin/helm" "$HOME/.local/bin/helm"
    cp "$build_dir/go/src/k8s.io/helm/bin/tiller" "$HOME/.local/bin/tiller"
    printe "kubectl has been successfully installed"
fi
