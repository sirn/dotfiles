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
        ffmpeg, \
        fzf, \
        git, \
        mercurial, \
        mosh, \
        oksh, \
        openssh, \
        pstree, \
        py38-ansible, \
        py38-tmuxp, \
        qemu +target_arm +target_riscv32 +target_riscv64 +target_x86_64, \
        rsync, \
        socat, \
        the_silver_searcher, \
        tmux, \
        tree, \
        unison -gtk, \
        w3m, \
        youtube-dl
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

    sh "$BASE_DIR/libexec/packages/lang/rust.sh" "$@"
    sh "$BASE_DIR/libexec/packages/lang/nim.sh" "$@"
}

_setup_nix_dev() {
    printe_h2 "Installing nix packages..."

    nix_bootstrap

    nix_ensure_channel "https://nixos.org/channels/nixpkgs-unstable"
    nix_install "$BASE_DIR/etc/nix/default.nix"
}
