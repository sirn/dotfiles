#!/bin/sh -e
#
# Install Ubuntu Linux packages with apt and snap.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../share/bootstrap/utils.sh"
. "../../share/bootstrap/utils_ubuntu.sh"

_run() {
    printe_h2 "Installing packages..."

    apt_bootstrap
    apt_setup_ppa ppa:dysfunctionalprogramming/oksh

    apt_install aptitude
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

    snap_install emacs --classic
}

_run_desktop() {
    printe_h2 "Installing desktop packages..."

    apt_setup_ppa ppa:unit193/encryption

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

    apt_install cabal-install
    apt_install duplicity
    apt_install elixir
    apt_install entr
    apt_install erlang
    apt_install git-crypt
    apt_install git-lfs
    apt_install graphicsmagick
    apt_install graphviz
    apt_install ipcalc
    apt_install jq
    apt_install leiningen
    apt_install pandoc
    apt_install python3
    apt_install python3-pip
    apt_install shellcheck
    apt_install socat
    apt_install tree

    snap_install go --classic
    snap_install google-cloud-sdk --classic
    snap_install node --classic --channel 10/stable
    snap_install ruby --classic
    snap_install shellcheck

    sh "$BASE_DIR/libexec/packages/cloudflared.sh" "$@"
    sh "$BASE_DIR/libexec/packages/haskell.sh" "$@"
    sh "$BASE_DIR/libexec/packages/node.sh" "$@"
    sh "$BASE_DIR/libexec/packages/python.sh" "$@"
    sh "$BASE_DIR/libexec/packages/rebar3.sh" "$@"
    sh "$BASE_DIR/libexec/packages/rust.sh" "$@"
    sh "$BASE_DIR/libexec/packages/terraform.sh" "$@"
}

_run_kubernetes() {
    printe_h2 "Installing kubernetes packages..."

    snap_install kubectl --classic
    snap_install helm --classic

    sh "$BASE_DIR/libexec/packages/kubectx.sh" "$@"
    sh "$BASE_DIR/libexec/packages/kapitan.sh" "$@"
}

_run_all() {
    run_with_flavors "$@"
}

_run_all "$@"
