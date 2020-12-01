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
        fzf, \
        git, \
        ipfs, \
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
        w3m

    ## Broken
    #   duplicity, \
    #   qemu +target_arm +target_riscv64, \
    #   unison -gtk, \
}
