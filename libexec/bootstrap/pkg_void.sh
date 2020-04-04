#!/bin/sh -e
#
# Install Void Linux packages with XBPS.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../share/bootstrap/utils.sh"
. "../../share/bootstrap/utils_void.sh"

_run() {
    printe_h2 "Installing packages..."

    xbps_install \
        aria2 \
        aspell \
        aspell-en \
        cronie \
        curl \
        fzf \
        git \
        loksh \
        mercurial \
        mosh \
        python3-tmuxp \
        rsync \
        socat \
        sqlite \
        the_silver_searcher \
        tmux \
        unzip \
        w3m \
        weechat \
        xtools \
        zip
}

_run_desktop() {
    printe_h2 "Installing desktop packages..."

    # Conflict with emacs
    if xbps_installed emacs; then
        run_root xbps-remove -Ry emacs
    fi

    xbps_install \
        emacs-gtk3 \
        firefox \
        qemu

    sh "$BASE_DIR/libexec/packages/fontinst.sh" "$@"
}

_run_dev() {
    printe_h2 "Installing dev packages..."

    xbps_install \
        GraphicsMagick \
        cabal-install \
        duplicity \
        elixir \
        entr \
        erlang \
        git-crypt \
        git-lfs \
        go \
        graphviz \
        ipcalc \
        jq \
        jsonnet \
        libressl-devel \
        nodejs-lts \
        pandoc \
        patch \
        python3 \
        python3-devel \
        python3-pip \
        python3-cryptography \
        ruby \
        shellcheck \
        socat \
        tcl \
        terraform \
        tree \
        xz

    sh "$BASE_DIR/libexec/packages/cloudflared.sh" "$@"
    sh "$BASE_DIR/libexec/packages/gcloud.sh" "$@"
    sh "$BASE_DIR/libexec/packages/kustomize.sh" "$@"
    sh "$BASE_DIR/libexec/packages/kubectx.sh" "$@"

    sh "$BASE_DIR/libexec/packages/erlang.sh" "$@"
    sh "$BASE_DIR/libexec/packages/elixir.sh" "$@"
    sh "$BASE_DIR/libexec/packages/golang.sh" "$@"
    sh "$BASE_DIR/libexec/packages/haskell.sh" "$@"
    sh "$BASE_DIR/libexec/packages/node.sh" "$@"
    sh "$BASE_DIR/libexec/packages/python.sh" "$@"
    sh "$BASE_DIR/libexec/packages/rust.sh" "$@"
}

_run_all() {
    run_with_flavors "$@"

    printe_h2 "Installing extra packages..."

    # Only install emacs-nox when other variant of Emacs hasn't been
    # installed (e.g. desktop flavor installs GTK emacs)
    if ! xbps_installed emacs-gtk3; then
        xbps_install emacs
    fi
}

_run_all "$@"
