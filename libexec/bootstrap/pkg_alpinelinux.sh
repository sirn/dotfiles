#!/bin/sh -e
#
# Install Alpine Linux packages with apk.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$BASE_DIR/share/bootstrap/funcs.sh"

# shellcheck source=../../share/bootstrap/alpinelinux.sh
. "$BASE_DIR/share/bootstrap/alpinelinux.sh"

_setup_env() {
    lineinfile \
        -S \
        -f /etc/apk/repositories \
        -l "@testing http://dl-cdn.alpinelinux.org/alpine/edge/testing" \
        -r "^@testing" \
        -s present
}

_run() {
    _setup_env

    printe_h2 "Installing packages..."

    # Conflict with ubase
    if _check_installed acct; then
        run_root apk del acct
    fi

    _do_apk aria2
    _do_apk aspell
    _do_apk aspell-en
    _do_apk build-base
    _do_apk curl
    _do_apk git
    _do_apk loksh
    _do_apk mercurial
    _do_apk mosh
    _do_apk openjdk8-jre
    _do_apk pstree
    _do_apk socat
    _do_apk sqlite
    _do_apk the_silver_searcher
    _do_apk tmux
    _do_apk ubase@testing
    _do_apk w3m
    _do_apk weechat
    _do_apk weechat-python
}

_run_desktop() {
    printe_h2 "Installing desktop packages..."

    # Conflict with emacs-x11
    if _check_installed emacs-nox; then
        run_root apk del emacs-nox
    fi

    _do_apk emacs-x11
    _do_apk feh
    _do_apk firefox@testing
    _do_apk font-noto
    _do_apk font-noto-emoji@testing
    _do_apk xset
    _do_apk xsetroot
    _do_apk xterm

    sh "$BASE_DIR/libexec/packages/fontinst.sh" "$@"
}

_run_all() {
    run_with_flavors "$@"

    # Only install emacs-nox when other variant of Emacs hasn't been
    # installed (e.g. desktop flavor installs emacs-x11)
    if ! _check_installed emacs-x11; then
        _do_apk emacs-nox
    fi
}

_run_all "$@"
