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

    apt_setup_ppa ppa:dysfunctionalprogramming/oksh
    apt_setup_ppa ppa:kelleyk/emacs

    apt_install aria2
    apt_install aspell
    apt_install aspell-en
    apt_install build-essential
    apt_install curl
    apt_install git
    apt_install oksh
    apt_install mercurial
    apt_install mosh
    apt_install openjdk-8-jre
    apt_install socat
    apt_install sqlite3
    apt_install silversearcher-ag
    apt_install tmux
    apt_install w3m
    apt_install weechat
}

_run_desktop() {
    printe_h2 "Installing desktop packages..."

    # Conflict with emacs-x11
    if apt_installed emacs25-nox; then
        run_root apt-get remove -y emacs25-nox
    fi

    apt_install emacs26
    snap_install firefox

    sh "$BASE_DIR/libexec/packages/fontinst.sh" "$@"
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
