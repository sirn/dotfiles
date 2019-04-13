#!/bin/sh -e
#
# Install rust packages.
#

root_dir=${BOOTSTRAP_ROOT:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}
lookup_dir=${LOOKUP_ROOT:-$root_dir}
flavors=$*

platform=$(uname | tr '[:upper:]' '[:lower:]')

# shellcheck source=../../share/bootstrap/funcs.sh
. "$root_dir/share/bootstrap/funcs.sh"


## Preparation
##

case $platform in
    openbsd )
        if command -v cargo >/dev/null; then
            printe_h2 "Preparing wxallowed for cargo..."

            # See also: https://deftly.net/posts/2017-10-12-using-cabal-on-openbsd.html
            if file_absent /usr/local/cargo; then
                run_root mkdir -p /usr/local/cargo
                run_root chown "$USER:wheel" /usr/local/cargo
            fi

            make_link /usr/local/cargo "$HOME/.cargo"
        else
            printe_h2 "Installing rust..."

            printe "Rustup is not available under OpenBSD"
            printe "Try \`pkg_add rust\`"
        fi
        ;;

    * )
        PATH=$HOME/.cargo/bin:$PATH

        printe_h2 "Installing rust..."

        if file_absent "$HOME/.cargo/bin/rustup"; then
            fetch_url - https://sh.rustup.rs | sh -s - -y --no-modify-path
        fi
        ;;
esac


## Fetch source
##

if command -v rustc >/dev/null; then
    rust_ver=$(rustc --version | awk '{ print $2 }')
    rust_src_base=$HOME/.local/lib/rustlib/src/rust

    printe_h2 "Preparing rust source code..."

    if file_absent "$rust_src_base/rust-$rust_ver"; then
        mkdir -p "$rust_src_base"
        fetch_gh_archive - "rust-lang/rust" "$rust_ver" | tar -C "$rust_src_base" -xzf -
    fi

    make_link "$rust_src_base/rust-$rust_ver/src" "$rust_src_base/src"
fi


## Install packages
##

rust_pkglist=$lookup_dir/var/bootstrap/pkglist_rust.txt

if command -v cargo >/dev/null; then
    for f in $(mangle_file "$rust_pkglist" "$platform" "$flavors"); do
        printe_h2 "Installing rust packages from ${f##$lookup_dir/}..."

        while read -r line; do
            case $line in
                "#"* | "" ) continue;;
                *) line=${line%%#*};;
            esac

            bin=${line%%:*}
            install=${line##$bin:}

            if is_force || file_absent "$HOME/.cargo/bin/$bin"; then
                # shellcheck disable=SC2086
                cargo install $install
            fi
        done < "$f"
    done
fi
