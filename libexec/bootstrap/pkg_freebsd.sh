#!/bin/sh -e
#
# Install FreeBSD packages with Pkgng.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$BASE_DIR/share/bootstrap/funcs.sh"

# shellcheck source=../../share/bootstrap/freebsd.sh
. "$BASE_DIR/share/bootstrap/freebsd.sh"

_setup_env() {
    if [ ! -x /usr/local/sbin/pkg ]; then
        printe_h2 "Bootstrapping pkgng..."
        run_root ASSUME_ALWAYS_YES=yes pkg bootstrap
    else
        printe_h2 "Updating pkg..."
        run_root pkg update -q
    fi
}

_run() {
    _setup_env

    printe_h2 "Installing packages..."
    _do_pkgng aria2
    _do_pkgng aspell
    _do_pkgng base64
    _do_pkgng ca_root_nss
    _do_pkgng compat8x-amd64
    _do_pkgng curl
    _do_pkgng en-aspell
    _do_pkgng git
    _do_pkgng mercurial
    _do_pkgng mosh
    _do_pkgng oksh
    _do_pkgng openjdk8-jre
    _do_pkgng pstree
    _do_pkgng socat
    _do_pkgng the_silver_searcher
    _do_pkgng tmux
    _do_pkgng w3m
    _do_pkgng weechat
}

_run_desktop() {
    printe_h2 "Installing desktop packages..."
    _do_pkgng cwm
    _do_pkgng emacs
    _do_pkgng firefox
    _do_pkgng noto
    _do_pkgng redshift
    _do_pkgng xset
    _do_pkgng xsetroot
    _do_pkgng xterm

    sh "$BASE_DIR/libexec/packages/fontinst.sh" "$@"
}

_run_dev() {
    printe_h2 "Installing dev packages..."
    _do_pkgng GraphicsMagick
    _do_pkgng autoconf
    _do_pkgng duplicity
    _do_pkgng elixir
    _do_pkgng entr
    _do_pkgng erlang
    _do_pkgng execline
    _do_pkgng expect
    _do_pkgng git-crypt
    _do_pkgng git-lfs
    _do_pkgng go
    _do_pkgng google-cloud-sdk
    _do_pkgng graphviz
    _do_pkgng hs-ShellCheck
    _do_pkgng hs-cabal-install
    _do_pkgng hs-pandoc
    _do_pkgng ipcalc
    _do_pkgng jq
    _do_pkgng leiningen
    _do_pkgng node10
    _do_pkgng npm-node10
    _do_pkgng pkgconf
    _do_pkgng py36-ansible
    _do_pkgng py36-asciinema
    _do_pkgng py36-pip
    _do_pkgng python3
    _do_pkgng python36
    _do_pkgng rebar3
    _do_pkgng ruby
    _do_pkgng socat
    _do_pkgng terraform
    _do_pkgng tree

    sh "$BASE_DIR/libexec/packages/cloudflared.sh" "$@"
    sh "$BASE_DIR/libexec/packages/haskell.sh" "$@"
    sh "$BASE_DIR/libexec/packages/node.sh" "$@"
    sh "$BASE_DIR/libexec/packages/python.sh" "$@"
    sh "$BASE_DIR/libexec/packages/rust.sh" "$@"
}

_run_kubernetes() {
    printe_h2 "Installing kubernetes packages..."
    _do_pkgng kubectl
    _do_pkgng helm

    sh "$BASE_DIR/libexec/packages/kubectx.sh" "$@"
    sh "$BASE_DIR/libexec/packages/kapitan.sh" "$@"
}

_run_all() {
    run_with_flavors "$@"

    # Only install emacs-nox when other variant of Emacs hasn't been
    # installed (e.g. desktop flavor installs emacs-x11)
    if ! _check_installed emacs; then
        _do_pkgng emacs-nox
    fi
}

_run_all "$@"
