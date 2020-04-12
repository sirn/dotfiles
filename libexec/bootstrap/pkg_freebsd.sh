#!/bin/sh -e
#
# Install FreeBSD packages with Pkgng.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../share/bootstrap/utils.sh"
. "../../share/bootstrap/utils_freebsd.sh"

_run() {
    printe_h2 "Installing packages..."

    pkgng_bootstrap

    pkgng_install \
        aria2 \
        aspell \
        base64 \
        ca_root_nss \
        compat8x-amd64 \
        curl \
        en-aspell \
        fzf \
        git \
        mercurial \
        mosh \
        oksh \
        openjdk8-jre \
        pstree \
        py37-ansible \
        py37-tmuxp \
        socat \
        the_silver_searcher \
        tmux \
        w3m
}

_run_desktop() {
    printe_h2 "Installing desktop packages..."

    pkgng_install \
        emacs \
        firefox

    sh "$BASE_DIR/libexec/packages/fontinst.sh" "$@"
}

_run_dev() {
    printe_h2 "Installing dev packages..."

    pkgng_install \
        GraphicsMagick \
        autoconf \
        duplicity \
        elixir \
        entr \
        erlang \
        execline \
        expect \
        git-crypt \
        git-lfs \
        go \
        graphviz \
        hs-ShellCheck \
        hs-cabal-install \
        hs-pandoc \
        ipcalc \
        jq \
        leiningen \
        node10 \
        npm-node10 \
        pkgconf \
        py37-pip \
        python37 \
        rebar3 \
        ruby \
        socat \
        terraform \
        tree

    sh "$BASE_DIR/libexec/packages/lang/rust.sh" "$@"
    sh "$BASE_DIR/libexec/packages/dev/erlang.sh" "$@"
    sh "$BASE_DIR/libexec/packages/dev/elixir.sh" "$@"
    sh "$BASE_DIR/libexec/packages/dev/golang.sh" "$@"
    sh "$BASE_DIR/libexec/packages/dev/haskell.sh" "$@"
    sh "$BASE_DIR/libexec/packages/dev/node.sh" "$@"
    sh "$BASE_DIR/libexec/packages/dev/python.sh" "$@"
    sh "$BASE_DIR/libexec/packages/net/cloudflared.sh" "$@"
    sh "$BASE_DIR/libexec/packages/net/kubernetes.sh" "$@"
}

_run_all() {
    run_with_flavors "$@"

    printe_h2 "Installing extra packages..."

    # Only install emacs-nox when other variant of Emacs hasn't been
    # installed (e.g. desktop flavor installs emacs-x11)
    if ! pkgng_installed emacs; then
        pkgng_install emacs-nox
    fi
}

_run_all "$@"
