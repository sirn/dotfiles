#!/bin/sh -e
#
# Install Darwin packages with MacPorts and MAS.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "lib/utils.sh"
. "lib/utils_darwin.sh"

_run() {
    printe_h2 "Installing packages..."

    macports_bootstrap

    macports_install \
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
        py37-ansible, \
        py37-tmuxp, \
        qemu +target_arm +target_riscv64, \
        rsync, \
        socat, \
        the_silver_searcher, \
        tmux, \
        w3m
}

_run_desktop() {
    printe_h2 "Installing desktop packages..."

    macports_install \
        emacs-mac-app, \
        mpv
}

_run_dev() {
    printe_h2 "Installing dev packages..."

    macports_install \
        GraphicsMagick, \
        autoconf, \
        carthage, \
        duplicity, \
        elixir, \
        entr, \
        erlang, \
        git-crypt, \
        git-lfs, \
        go, \
        graphviz, \
        hs-cabal-install, \
        ipcalc, \
        jq, \
        leiningen, \
        nodejs10, \
        npm6, \
        pandoc, \
        py37-pip, \
        python37, \
        rebar3, \
        ruby26, \
        shellcheck, \
        socat, \
        tcl, \
        terraform, \
        tree, \
        xz, \
        zlib

    macports_select pip3 pip37
    macports_select python3 python37
    macports_select ruby ruby26

    sh "$BASE_DIR/var/dotfiles/packages/lang/rust.sh" "$@"
    sh "$BASE_DIR/var/dotfiles/packages/lang/nim.sh" "$@"
    sh "$BASE_DIR/var/dotfiles/packages/dev/erlang.sh" "$@"
    sh "$BASE_DIR/var/dotfiles/packages/dev/elixir.sh" "$@"
    sh "$BASE_DIR/var/dotfiles/packages/dev/golang.sh" "$@"
    sh "$BASE_DIR/var/dotfiles/packages/dev/haskell.sh" "$@"
    sh "$BASE_DIR/var/dotfiles/packages/dev/node.sh" "$@"
    sh "$BASE_DIR/var/dotfiles/packages/dev/python.sh" "$@"
    sh "$BASE_DIR/var/dotfiles/packages/net/cloudflared.sh" "$@"
    sh "$BASE_DIR/var/dotfiles/packages/net/kubernetes.sh" "$@"
}
