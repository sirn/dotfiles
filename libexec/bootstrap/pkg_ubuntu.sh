#!/bin/sh -e
#
# Install Ubuntu Linux packages with apt.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../share/bootstrap/utils.sh"
. "../../share/bootstrap/utils_ubuntu.sh"

_run() {
    printe_h2 "Installing packages..."

    apt_bootstrap
    apt_setup_ppa ppa:dysfunctionalprogramming/oksh
    apt_setup_ppa ppa:kelleyk/emacs

    apt_install aria2
    apt_install aspell
    apt_install aspell-en
    apt_install build-essential
    apt_install curl
    apt_install git
    apt_install mercurial
    apt_install mosh
    apt_install oksh
    apt_install openjdk-8-jre
    apt_install ovmf
    apt_install qemu
    apt_install silversearcher-ag
    apt_install socat
    apt_install sqlite3
    apt_install tmux
    apt_install tmuxp
    apt_install unzip
    apt_install w3m
    apt_install weechat
}

_run_desktop() {
    printe_h2 "Installing desktop packages..."

    apt_setup_ppa ppa:unit193/encryption

    # Conflict with emacs-x11
    if apt_installed emacs26-nox; then
        run_root apt-get remove -y emacs26-nox
    fi

    apt_install emacs26
    apt_install firefox
    apt_install virt-manager
    apt_install veracrypt

    sh "$BASE_DIR/libexec/packages/fontinst.sh" "$@"
}

_run_dev() {
    printe_h2 "Installing dev packages..."

    apt_setup_repo \
        https://packages.erlang-solutions.com/ubuntu/erlang_solutions.asc \
        https://packages.erlang-solutions.com/ubuntu LSB_RELEASE contrib

    apt_setup_repo \
        https://deb.nodesource.com/gpgkey/nodesource.gpg.key \
        https://deb.nodesource.com/node_10.x LSB_RELEASE main

    apt_setup_repo \
        https://packages.cloud.google.com/apt/doc/apt-key.gpg \
        http://packages.cloud.google.com/apt cloud-sdk main

    apt_setup_repo \
        https://download.docker.com/linux/ubuntu/gpg \
        https://download.docker.com/linux/ubuntu LSB_RELEASE stable

    apt_setup_ppa ppa:brightbox/ruby-ng
    apt_setup_ppa ppa:longsleep/golang-backports

    apt_install cabal-install
    apt_install containerd.io+M
    apt_install docker-ce
    apt_install docker-ce-cli+M
    apt_install duplicity
    apt_install elixir
    apt_install entr
    apt_install erlang
    apt_install git-crypt
    apt_install git-lfs
    apt_install golang-go
    apt_install google-cloud-sdk
    apt_install graphicsmagick
    apt_install graphviz
    apt_install ipcalc
    apt_install jq
    apt_install leiningen
    apt_install nodejs
    apt_install pandoc
    apt_install python3
    apt_install python3-pip
    apt_install python3-venv
    apt_install ruby2.6
    apt_install shellcheck
    apt_install socat
    apt_install tree

    sh "$BASE_DIR/libexec/packages/cloudflared.sh" "$@"
    sh "$BASE_DIR/libexec/packages/haskell.sh" "$@"
    sh "$BASE_DIR/libexec/packages/node.sh" "$@"
    sh "$BASE_DIR/libexec/packages/python.sh" "$@"
    sh "$BASE_DIR/libexec/packages/rebar3.sh" "$@"
    sh "$BASE_DIR/libexec/packages/rust.sh" "$@"
    sh "$BASE_DIR/libexec/packages/terraform.sh" "$@"
}

_run_all() {
    run_with_flavors "$@"

    # Only install emacs-nox when other variant of Emacs hasn't been
    # installed (e.g. desktop flavor installs emacs)
    if ! apt_installed emacs26; then
        apt_install emacs26-nox
    fi
}

_run_all "$@"
