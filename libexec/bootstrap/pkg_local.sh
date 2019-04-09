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


## Utils
##

# See also: https://deftly.net/posts/2017-10-12-using-cabal-on-openbsd.html
_prepare_wxallowed() {
    name=$1; shift
    link=$1; shift

    case "$platform" in
        openbsd )
            wxdir="/usr/local/$name"

            printe_info "$name requires wxallowed, setting up $wxdir..."

            if [ ! -d "$wxdir" ]; then
                run_root mkdir -p "$wxdir"
                run_root chown "$USER:wheel" "$wxdir"
            fi

            if [ "$(readlink "$link")" = "$wxdir" ] ; then
                printe "$link already linked"
            else
                rm -rf "$link"
                ln -s "$wxdir" "$link"
                printe "$link has been linked to $wxdir"
            fi
            ;;
        * )
            ;;
    esac
}


## Rust
##

printe_h2 "Installing rust..."

case "$platform" in
    openbsd )
        if ! command -v cargo >/dev/null; then
            printe "Rustup is not available under OpenBSD"
            printe "Try \`pkg_add rust\`"
        fi
        ;;

    * )
        PATH="$HOME/.cargo/bin:$PATH"

        if [ ! -x "$HOME/.cargo/bin/rustup" ]; then
            fetch_url - https://sh.rustup.rs | sh -s - -y --no-modify-path
        fi
        ;;
esac

if command -v rustc >/dev/null; then
    rust_ver="$(rustc --version | awk '{ print $2 }')"
    rust_src_base="$HOME/.local/lib/rustlib/src/rust"

    if [ ! -d "$rust_src_base/rust-${rust_ver}" ]; then
        mkdir -p "$rust_src_base"
        fetch_url - "https://github.com/rust-lang/rust/archive/${rust_ver}.tar.gz" | tar -C "$rust_src_base" -xzf -
    fi

    if [ "$(readlink "$rust_src_base/src")" = "$rust_src_base/rust-${rust_ver}/src" ]; then
        printe "$rust_src_base/src already linked"
    else
        rm -rf "$rust_src_base/src"
        ln -s "$rust_src_base/rust-${rust_ver}/src" "$rust_src_base/src"
        printe "$rust_src_base/src has been linked to $rust_src_base/rust-${rust_ver}/src"
    fi
fi

rust_pkglist="../../var/bootstrap/pkglist_rust.txt"

if command -v cargo >/dev/null; then
    _prepare_wxallowed cargo "$HOME/.cargo"

    for f in $(mangle_file "$rust_pkglist" "$platform" "$flavors"); do
        printe_h2 "Installing rust packages from ${f##../../}..."

        while read -r spec; do
            case "$spec" in
                "#"* | "" ) continue;;
                *) spec="${spec%%#*}";;
            esac

            bin="${spec%%:*}"
            install="${spec##$bin:}"

            if [ -f "$HOME/.cargo/bin/$bin" ]; then
                printe "$bin already installed"
                continue
            fi

            eval cargo install "$install"
        done < "$f"
    done
fi


## Node
##

case "$platform" in
    darwin )
        PATH="/usr/local/opt/node@10/bin:$PATH"
        ;;

    * )
        ;;
esac

node_pkglist="../../var/bootstrap/pkglist_node.txt"

if command -v npm >/dev/null; then
    npm set prefix="$HOME/.local"

    for f in $(mangle_file "$node_pkglist" "$platform" "$flavors"); do
        printe_h2 "Installing node packages from ${f##../../}..."
        xargs npm install -g < "$f"
    done
fi


## Haskell
##

haskell_pkglist="../../var/bootstrap/pkglist_haskell.txt"

if command -v cabal >/dev/null; then
    _prepare_wxallowed cabal "$HOME/.cabal"

    case "$platform" in
        openbsd )
            mkdir -p "$HOME/.cabal/build"

            _haskell_cabal() {
                env TMPDIR=/usr/local/cabal/build cabal "$@"
            }
            ;;

        * )
            _haskell_cabal() {
                cabal "$@"
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
        _haskell_cabal "${haskell_cabal_prefix}update"
    fi

    for f in $(mangle_file "$haskell_pkglist" "$platform" "$flavors"); do
        printe_h2 "Installing haskell cabal packages from ${f##../../}..."

        while read -r spec; do
            case "$spec" in
                "#"* | "" ) continue;;
                *) spec="${spec%%#*}";;
            esac

            bin="${spec%%:*}"
            install="${spec##$bin:}"

            if [ -f "$HOME/.cabal/bin/$bin" ]; then
                printe "$bin already installed"
                continue
            fi

            eval _haskell_cabal "${haskell_cabal_prefix}install" "$install"
        done < "$f"
    done
fi


## Kubernetes flavor
##

if [ "$(has_args "kubernetes" "$flavors")" = "1" ]; then
    printe_h2 "Installing kubectx..."

    git_clone_update https://github.com/ahmetb/kubectx.git "$HOME/.local/src/kubectx"
    ln -fs "$HOME/.local/src/kubectx/kubectx" "$HOME/.local/bin/kubectx"
    ln -fs "$HOME/.local/src/kubectx/kubens" "$HOME/.local/bin/kubens"

    printe_h2 "Installing kapitan..."

    env \
        CXXFLAGS="-fPIC -Iinclude -Ithird_party/md5 -Ithird_party/json -std=c++11" \
        "$HOME/.asdf/shims/pip3" install -U kapitan

    "$HOME/.asdf/bin/asdf" reshim python
fi
