#!/bin/sh -e
#
# Install Alpine Linux packages with apk.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../share/bootstrap/utils.sh"
. "../../share/bootstrap/utils_alpine.sh"

_run() {
    printe_h2 "Installing packages..."

    lineinfile \
        -S \
        -f /etc/apk/repositories \
        -l "@testing http://dl-cdn.alpinelinux.org/alpine/edge/testing" \
        -r "^@testing" \
        -s present

    apk_install aria2
    apk_install aspell
    apk_install aspell-en
    apk_install build-base
    apk_install curl
    apk_install git
    apk_install loksh
    apk_install mercurial
    apk_install mosh
    apk_install openjdk8-jre
    apk_install pstree
    apk_install socat
    apk_install sqlite
    apk_install the_silver_searcher
    apk_install tmux
    apk_install w3m
    apk_install weechat
    apk_install weechat-python
}

_run_desktop() {
    printe_h2 "Installing desktop packages..."

    # Conflict with emacs-x11
    if apk_installed emacs-nox; then
        run_root apk del emacs-nox
    fi

    apk_install cwm
    apk_install emacs-x11
    apk_install feh
    apk_install firefox@testing
    apk_install font-noto
    apk_install font-noto-emoji@testing
    apk_install redshift
    apk_install xset
    apk_install xsetroot
    apk_install xterm

    sh "$BASE_DIR/libexec/packages/fontinst.sh" "$@"
}

_run_all() {
    run_with_flavors "$@"

    # Only install emacs-nox when other variant of Emacs hasn't been
    # installed (e.g. desktop flavor installs emacs-x11)
    if ! apk_installed emacs-x11; then
        apk_install emacs-nox
    fi
}

_run_all "$@"
