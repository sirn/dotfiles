#!/bin/sh -e
#
# Install Darwin packages with MacPorts and MAS.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$BASE_DIR/share/bootstrap/funcs.sh"

# shellcheck source=../../share/bootstrap/darwin.sh
. "$BASE_DIR/share/bootstrap/darwin.sh"

if [ -z "$BUILD_DIR" ]; then
    BUILD_DIR=$(mktemp -d)
    trap 'rm -rf $BUILD_DIR' 0 1 2 3 6 14 15
fi

MACPORTS_VER=2.5.4
MACPORTS_SHA256=592e4a021588f37348fe7b806c202e4d77f75bcff1a0b20502d5f1177c2c21ff

_setup_env() {
    if is_force || [ ! -x $MACPORTS ]; then
        printe_h2 "Bootstrapping MacPorts..."

        xcode-select --install 2>/dev/null || true
        cd "$BUILD_DIR" || exit 1

        fetch_gh_archive macports.tar.gz macports/macports-base v$MACPORTS_VER
        verify_shasum macports.tar.gz $MACPORTS_SHA256
        tar -C "$BUILD_DIR" -xzf macports.tar.gz
        rm macports.tar.gz

        cd "$BUILD_DIR/macports-base-$MACPORTS_VER" || exit 1
        ./configure && make
        run_root make install
        run_root $MACPORTS sync
    fi

    if version_gte "$MAS_PLATFORM" "$PLATFORM_VERS"; then
        if is_force || [ ! -x $MAS ]; then
            printe_h2 "Bootstrapping mas..."
            run_root $MACPORTS -Nf install mas
        fi
    else
        printe_info "mas command line utility requires macOS <= $MAS_PLATFORM"
    fi
}

_run() {
    _setup_env

    printe_h2 "Installing packages..."
    _do_macports aria2 +sqlite3
    _do_macports curl +darwinssl +http2
    _do_macports dnscrypt-proxy
    _do_macports emacs
    _do_macports git
    _do_macports ipfs
    _do_macports mercurial
    _do_macports mosh
    _do_macports oksh
    _do_macports openssh
    _do_macports pstree
    _do_macports socat
    _do_macports the_silver_searcher
    _do_macports tmux
    _do_macports w3m
}

_run_desktop() {
    printe_h2 "Installing desktop packages..."
    _do_macports emacs-mac-app

    _do_mas 407963104 Pixelmator
    _do_mas 411643860 DaisyDisk
    _do_mas 413965349 Soulver
    _do_mas 497799835 Xcode
    _do_mas 603637384 Name Mangler 3
    _do_mas 775737590 iA Writer
    _do_mas 975937182 Fantastical 2
    _do_mas 1333542190 1Password 7
    _do_mas 1435957248 Drafts
}

_run_dev() {
    printe_h2 "Installing dev packages..."
    _do_macports GraphicsMagick
    _do_macports autoconf
    _do_macports carthage
    _do_macports duplicity
    _do_macports elixir
    _do_macports entr
    _do_macports erlang
    _do_macports git-crypt
    _do_macports git-lfs
    _do_macports go
    _do_macports google-cloud-sdk
    _do_macports graphviz
    _do_macports hs-cabal-install
    _do_macports ipcalc
    _do_macports jq
    _do_macports leiningen
    _do_macports nodejs10
    _do_macports npm6
    _do_macports pandoc
    _do_macports py37-ansible
    _do_macports py37-pip
    _do_macports python37
    _do_macports rebar3
    _do_macports ruby26
    _do_macports socat
    _do_macports terraform
    _do_macports tree
    _do_macports xz
    _do_macports zlib

    # Outdated
    # https://github.com/macports/macports-ports/pull/4577
    #_do_macports shellcheck

    printe_h2 "Installing default versions..."
    run_root port select ansible py37-ansible
    run_root port select pip3 pip37
    run_root port select python3 python37
    run_root port select ruby ruby26

    sh "$BASE_DIR/libexec/packages/cloudflared.sh" "$@"
    sh "$BASE_DIR/libexec/packages/haskell.sh" "$@"
    sh "$BASE_DIR/libexec/packages/node.sh" "$@"
    sh "$BASE_DIR/libexec/packages/python.sh" "$@"
    sh "$BASE_DIR/libexec/packages/rust.sh" "$@"
}

_run_kubernetes() {
    printe_h2 "Installing kubernetes packages..."
    _do_macports kubectl
    _do_macports helm

    sh "$BASE_DIR/libexec/packages/kubectx.sh" "$@"
    sh "$BASE_DIR/libexec/packages/kapitan.sh" "$@"
}

run_with_flavors "$@"
