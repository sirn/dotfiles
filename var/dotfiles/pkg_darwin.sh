#!/bin/sh -e
#
# Install Darwin packages with MacPorts and MAS.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "lib/utils.sh"
. "lib/utils_darwin.sh"
. "lib/utils_nix.sh"
. "lib/buildenv.sh"

_run() {
    printe_h2 "Installing packages..."

    macports_bootstrap

    macports_install \
        GraphicsMagick, \
        aria2 +sqlite3, \
        aspell, \
        aspell-dict-en, \
        curl +darwinssl +http2, \
        duplicity, \
        emacs, \
        fzf, \
        git, \
        ipfs, \
        mercurial, \
        mosh, \
        oksh, \
        openssh, \
        pstree, \
        py37-ansible, \
        py37-tmuxp, \
        qemu +target_arm +target_riscv64, \
        rsync, \
        socat, \
        the_silver_searcher, \
        tmux, \
        tree, \
        unison, \
        w3m
}

_run_desktop() {
    printe_h2 "Installing desktop packages..."

    macports_install \
        emacs-mac-app
}

_run_dev() {
    _setup_macports_dev "$@"
    _setup_nix_dev "$@"
}

_setup_macports_dev() {
    printe_h2 "Installing dev packages..."

    macports_install \
        carthage

    sh "$BASE_DIR/var/dotfiles/packages/lang/rust.sh" "$@"
    sh "$BASE_DIR/var/dotfiles/packages/lang/nim.sh" "$@"
}

_setup_nix_dev() {
    printe_h2 "Installing nix packages..."

    nix_bootstrap

    nix_ensure_channel "https://nixos.org/channels/nixpkgs-20.09-darwin"
    nix_install "$BASE_DIR/etc/nix/default.nix"
}
