#!/bin/sh -e
#
# Install Arch Linux packages with Pacman and AUR.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../share/bootstrap/utils.sh"
. "../../share/bootstrap/utils_arch.sh"

_run() {
    printe_h2 "Installing packages..."

    aur_bootstrap

    pacman_install aria2
    pacman_install aspell
    pacman_install aspell-en
    pacman_install curl
    pacman_install git
    pacman_install mercurial
    pacman_install mosh
    pacman_install socat
    pacman_install sqlite
    pacman_install the_silver_searcher
    pacman_install tmux
    pacman_install w3m
    pacman_install weechat

    aur_install oksh
}

_run_desktop() {
    printe_h2 "Installing desktop packages..."

    # Conflict with emacs-x11
    if pacman_installed emacs-nox; then
        run_root pacman -Rns --noconfirm emacs-nox
    fi

    pacman_install emacs
    pacman_install firefox

    sh "$BASE_DIR/libexec/packages/fontinst.sh" "$@"
}

_run_dev() {
    printe_h2 "Installing dev packages..."

    pacman_install ansible
    pacman_install cabal-install
    pacman_install duplicity
    pacman_install elixir
    pacman_install entr
    pacman_install erlang
    pacman_install git-crypt
    pacman_install git-lfs
    pacman_install go
    pacman_install graphicsmagick
    pacman_install graphviz
    pacman_install ipcalc
    pacman_install jq
    pacman_install nodejs-lts-dubnium
    pacman_install npm
    pacman_install pandoc
    pacman_install python
    pacman_install python-pip
    pacman_install ruby
    pacman_install shellcheck
    pacman_install socat
    pacman_install terraform
    pacman_install tree
    pacman_install xz

    aur_install google-cloud-sdk

    sh "$BASE_DIR/libexec/packages/cloudflared.sh" "$@"
    sh "$BASE_DIR/libexec/packages/haskell.sh" "$@"
    sh "$BASE_DIR/libexec/packages/node.sh" "$@"
    sh "$BASE_DIR/libexec/packages/python.sh" "$@"
    sh "$BASE_DIR/libexec/packages/rebar3.sh" "$@"
    sh "$BASE_DIR/libexec/packages/rust.sh" "$@"
}

_run_all() {
    run_with_flavors "$@"

    # Only install emacs-nox when other variant of Emacs hasn't been
    # installed (e.g. desktop flavor installs GTK emacs)
    if ! pacman_installed emacs; then
        pacman_install emacs-nox
    fi
}

_run_kubernetes() {
    printe_h2 "Installing kubernetes packages..."

    pacman_install kubectl
    aur_install kubernetes-helm

    sh "$BASE_DIR/libexec/packages/kubectx.sh" "$@"
    sh "$BASE_DIR/libexec/packages/kapitan.sh" "$@"
}

_run_all "$@"
