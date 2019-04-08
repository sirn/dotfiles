#!/bin/sh -e
#
# Install a package manually and locally.
#

base_dir=$(cd "$(dirname "$0")/" || exit; pwd -P)
platform=$(uname | tr '[:upper:]' '[:lower:]')
flavors=$*

cd "$base_dir" || exit 1
. ../../share/bootstrap/funcs.sh
. ../../share/bootstrap/compat.sh


## Rust
##

printe_h2 "Installing rust..."

if [ ! -x "$HOME/.cargo/bin/rustup" ]; then
    fetch_url - https://sh.rustup.rs | sh -s - -y --no-modify-path
fi

"$HOME/.cargo/bin/rustup" update
"$HOME/.cargo/bin/rustup" component add rust-src
"$HOME/.cargo/bin/rustup" component add rustfmt-preview

rust_pkglist="../../var/bootstrap/pkglist.rust.txt"

for f in $(mangle_file "$rust_pkglist" "$platform" "$flavors"); do
    printe_h2 "Installing rust packages from ${f##../../}..."

    while read -r spec; do
        bin="${spec%%:*}"
        install="${spec##$bin:}"

        if [ -f "$HOME/.cargo/bin/$bin" ]; then
            printe "$bin already installed"
            continue
        fi

        eval "$HOME/.cargo/bin/cargo" install "$install"
    done < "$f"
done


## Node
##

_node_env() {
    case "$platform" in
        darwin ) env PATH="/usr/local/opt/node@10/bin:$PATH" "$@";;
        * ) "$@";;
    esac
}

_node_env npm set prefix="$HOME/.local"

node_pkglist="../../var/bootstrap/pkglist.node.txt"

for f in $(mangle_file "$node_pkglist" "$platform" "$flavors"); do
    printe_h2 "Installing node packages from ${f##../../}..."
    _node_env xargs npm install -g < "$f"
done


## Haskell and friends
##

printe_h2 "Installing haskell..."

case "$platform" in
    freebsd )
        printe "Haskell Stack is not available under FreeBSD at this moment"
        printe "See also https://github.com/commercialhaskell/stack/issues/4503"
        ;;

    * )
        # Haskell Stack
        #

        if [ -x "$HOME/.local/bin/stack" ]; then
            "$HOME/.local/bin/stack" update
        else
            fetch_url - https://get.haskellstack.org/ | sh -s - -d "$HOME/.local/bin"
        fi

        # PureScript
        #

        printe_h2 "Installing purescript..."
        _node_env npm install -g pulp purescript
        ;;
esac


## Kubernetes flavor
##

if [ "$(has_args "kubernetes" "$flavors")" = "1" ]; then
    if ! hash clang 2>/dev/null; then
        printe_err "Kubernetes flavor requires Clang"
        exit 1
    fi

    # Kubectx
    #

    printe_h2 "Installing kubectx..."

    git_clone_update https://github.com/ahmetb/kubectx.git "$HOME/.local/src/kubectx"
    ln -fs "$HOME/.local/src/kubectx/kubectx" "$HOME/.local/bin/kubectx"
    ln -fs "$HOME/.local/src/kubectx/kubens" "$HOME/.local/bin/kubens"

    # Kapitan
    #

    printe_h2 "Installing kapitan..."

    env \
        CC=clang \
        CXX=clang++ \
        CXXFLAGS="-fPIC -Iinclude -Ithird_party/md5 -Ithird_party/json -std=c++11" \
        "$HOME/.asdf/shims/pip3" install -U kapitan

    "$HOME/.asdf/bin/asdf" reshim python3
fi
