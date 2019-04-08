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

case "$platform" in
    openbsd )
        _rust_env() {
            "$@"
        }

        if ! hash cargo 2>/dev/null; then
            printe "Rustup is (unfortunately) not available for OpenBSD"
            printe "You may still install rust with \`pkg_add rust\`"
        else
            if [ ! -f "$HOME/.cargo/bin/rustfmt" ]; then
                _rust_env cargo install rustfmt
            else
                printe "rustfmt already installed"
            fi

            rust_ver="$(rustc --version | awk '{ print $2 }')"
            rust_src_base="$HOME/lib/rustlib/src/rust"
            rust_src_url="https://github.com/rust-lang/rust/archive/${rust_ver}.tar.gz"
            rust_src_ver="$rust_src_base/rust-${rust_ver}"

            if [ ! -d "$rust_src_ver" ]; then
                mkdir -p "$rust_src_base"
                fetch_url - "$rust_src_url" | tar -C "$rust_src_base" -xzf -
            fi

            if [ "$(readlink "$rust_src_base/src")" = "$rust_src_ver/src" ]; then
                printe "rust-src is already linked"
            else
                rm -rf "$rust_src_base/src"
                ln -s "$rust_src_ver/src" "$rust_src_base/src"
                printe "rust-src has been linked to $rust_src_ver"
            fi
        fi
        ;;
    * )
        _rust_env() {
            env PATH="$HOME/.cargo/bin:$PATH" "$@"
        }

        if [ ! -x "$HOME/.cargo/bin/rustup" ]; then
            fetch_url - https://sh.rustup.rs | sh -s - -y --no-modify-path
        fi

        _rust_env rustup update
        _rust_env rustup component add rust-src
        _rust_env rustup component add rustfmt
        ;;
esac

_rust_env; if hash cargo 2>/dev/null; then
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

            eval _rust_env cargo install "$install"
        done < "$f"
    done
fi


## Node
##

case "$platform" in
    darwin )
        _node_env() {
            env PATH="/usr/local/opt/node@10/bin:$PATH" "$@"
        }
        ;;

    * )
        _node_env() {
            "$@"
        }
        ;;
esac

_node_env npm set prefix="$HOME/.local"

node_pkglist="../../var/bootstrap/pkglist.node.txt"

for f in $(mangle_file "$node_pkglist" "$platform" "$flavors"); do
    printe_h2 "Installing node packages from ${f##../../}..."
    _node_env xargs npm install -g < "$f"
done


## Haskell and friends
##

haskell_pkglist="../../var/bootstrap/pkglist.haskell.txt"

case "$platform" in
    openbsd )
        _haskell_env() {
            env TMPDIR=/usr/local/cabal/build "$@"
        }

        # Cabal packages need to be built in /usr/local/cabal on OpenBSD
        # https://deftly.net/posts/2017-10-12-using-cabal-on-openbsd.html
        if [ ! -d /usr/local/cabal/build ]; then
            run_root mkdir -p /usr/local/cabal/build
            run_root chown -R "$USER:wheel" /usr/local/cabal
        fi

        if [ ! -L "$HOME/.cabal" ]; then
            rm -rf "$HOME/.cabal"
            ln -s /usr/local/cabal "$HOME/.cabal"
        fi
        ;;

    * )
        _haskell_env() {
            "$@"
        }
        ;;
esac

# Cabal >= 2.4.0.0 replaces update/install with v1-update/v1-install
# See https://github.com/haskell/cabal/blob/master/cabal-install/changelog
haskell_cabal_prefix=""

if [ "$(version_gte "2.4.0.0" "$(cabal --numeric-version)")" = "1" ]; then
    haskell_cabal_prefix="v1-"
fi

if [ ! -d "$HOME/.cabal/packages/hackage.haskell.org" ]; then
    printe_h2 "Updating haskell cabal package index..."
    _haskell_env cabal "${haskell_cabal_prefix}update"
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

        eval _haskell_env cabal "${haskell_cabal_prefix}install" "$install"
    done < "$f"
done


## Kubernetes flavor
##

if [ "$(has_args "kubernetes" "$flavors")" = "1" ]; then
    if ! hash clang 2>/dev/null; then
        printe_err "Kubernetes flavor requires clang"
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
