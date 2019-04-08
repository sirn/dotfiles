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

haskell_pkglist="../../var/bootstrap/pkglist.haskell.txt"

_haskell_env() {
    case "$platform" in
        openbsd )
            # Cabal packages need to be built in /usr/local/cabal on OpenBSD
            # https://deftly.net/posts/2017-10-12-using-cabal-on-openbsd.html
            if [ ! -d /usr/local/cabal/build ]; then
                run_root mkdir -p /usr/local/cabal/build
                run_root chown -R $USER:wheel /usr/local/cabal
            fi

            if [ ! -L "$HOME/.cabal" ]; then
                rm -rf "$HOME/.cabal"
                ln -s /usr/local/cabal "$HOME/.cabal"
            fi

            env TMPDIR=/usr/local/cabal/build "$@"
            ;;
        * ) "$@";;
    esac
}

if [ ! -d "$HOME/.cabal/packages/hackage.haskell.org" ]; then
    printe_h2 "Updating haskell cabal package index..."
    _haskell_env cabal v1-update
fi

for f in $(mangle_file "$haskell_pkglist" "$platform" "$flavors"); do
    printe_h2 "Installing haskell cabal packages from ${f##../../}..."

    while read -r spec; do
        bin="${spec%%:*}"
        install="${spec##$bin:}"

        if [ -f "$HOME/.cabal/bin/$bin" ]; then
            printe "$bin already installed"
            continue
        fi

        eval _haskell_env cabal v1-install "$install"
    done < "$f"
done


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
