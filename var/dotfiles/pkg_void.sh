#!/bin/sh -e
#
# Install Void Linux packages with XBPS.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "lib/utils.sh"
. "lib/utils_void.sh"

_run() {
    printe_h2 "Installing packages..."

    xbps_install \
        ansible \
        aria2 \
        aspell \
        aspell-en \
        cronie \
        curl \
        fzf \
        git \
        iptables-nft \
        loksh \
        mercurial \
        mosh \
        neovim \
        podman \
        podman-compose \
        python3-tmuxp \
        rsync \
        socat \
        sqlite \
        the_silver_searcher \
        tmux \
        w3m \
        xtools

    xbps_alternative iptables iptables-nft
    xbps_alternative vim neovim
    xbps_alternative vi neovim
}

_run_desktop() {
    printe_h2 "Installing desktop packages..."

    # Conflict with emacs
    if xbps_installed emacs; then
        run_root xbps-remove -Ry emacs
    fi

    # Firefox is installed with Flatpak
    xbps_install \
        emacs-gtk3 \
        qemu

    sh "$BASE_DIR/libexec/packages/fontinst.sh" "$@"
}

_run_dev() {
    printe_h2 "Installing dev packages..."

    xbps_install \
        GraphicsMagick \
        cabal-install \
        choosenim \
        duplicity \
        elixir \
        entr \
        erlang \
        erlang-wx \
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
        ruby \
        shellcheck \
        socat \
        tcl \
        terraform \
        tree \
        xz

    sh "$BASE_DIR/libexec/packages/lang/rust.sh" "$@"
    sh "$BASE_DIR/libexec/packages/lang/nim.sh" "$@"
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
    # installed (e.g. desktop flavor installs GTK emacs)
    if ! xbps_installed emacs-gtk3; then
        xbps_install emacs
    fi
}

_run_all "$@"
