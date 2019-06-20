#!/bin/sh -e
#
# Install rust packages.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$BASE_DIR/share/bootstrap/funcs.sh"

_setup_rust() {
    case $(uname) in
        OpenBSD )
            # https://deftly.net/posts/2017-10-12-using-cabal-on-openbsd.html
            if command -v cargo >/dev/null; then
                printe_h2 "Preparing wxallowed for cargo..."

                if file_absent /usr/local/cargo; then
                    run_root mkdir -p /usr/local/cargo
                    run_root chown "$USER:wheel" /usr/local/cargo
                fi

                make_link /usr/local/cargo "$HOME/.cargo"
            else
                printe_h2 "Installing rust..."

                printe_info "Rustup is not available under OpenBSD"
                printe_info "Try \`pkg_add rust\`"
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
}

_setup_rust_src() {
    if command -v rustc >/dev/null; then
        rust_ver=$(rustc --version | awk '{ print $2 }')
        rust_src_base=$HOME/.local/lib/rustlib/src/rust

        printe_h2 "Preparing rust source code..."

        if file_absent "$rust_src_base/rust-$rust_ver"; then
            mkdir -p "$rust_src_base"
            fetch_gh_archive - "rust-lang/rust" "$rust_ver" |
                tar -C "$rust_src_base" -xzf -
        fi

        make_link "$rust_src_base/rust-$rust_ver/src" "$rust_src_base/src"
    fi
}

_do_cargo_install() {
    bin=$1; shift

    if is_force || file_absent "$HOME/.cargo/bin/$bin"; then
        cargo install "$@"
    fi
}

_run() {
    _setup_rust
    _setup_rust_src

    if ! command -v cargo >/dev/null; then
        printe_info "cargo is not installed, skipping..."
        return 1
    fi
}

_run_dev() {
    # racer >= 2.1 requires nightly which we cannot use
    printe_h2 "Installing rust dev packages..."
    _do_cargo_install racer racer --vers ~2.0.0
    _do_cargo_install rustfmt rustfmt
}

run_with_flavors "$@"
