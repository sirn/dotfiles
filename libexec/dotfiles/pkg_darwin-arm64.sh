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
        rsync, \
        socat, \
        the_silver_searcher, \
        tmux, \
        tree, \
        w3m, \
        youtube-dl

    ## Broken
    #   duplicity, \
    #   qemu +target_arm +target_riscv64, \
    #   unison -gtk, \
}

_run_desktop() {
    printe_h2 "Installing desktop packages..."

    macports_install \
        emacs-mac-app
}

_run_dev() {
    _setup_macports_dev "$@"
    _setup_nix_dev "$@"

    sh "$BASE_DIR/libexec/packages/lang/rust.sh" "$@"
}

_setup_macports_dev() {
    printe_h2 "Installing dev packages..."

    macports_install \
        carthage
}

_setup_nix_dev() {
    printe_h2 "Installing nix packages..."

    nix_bootstrap

    nix_ensure_channel "https://nixos.org/channels/nixpkgs-unstable"
    nix_install "$BASE_DIR/etc/nix/default.nix"
}
