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
        curl \
        execline \
        fzf \
        git \
        oksh \
        mercurial \
        mosh \
        neovim \
        podman \
        podman-compose \
        python3-tmuxp \
        rsync \
        s6 \
        snooze \
        socat \
        sqlite \
        the_silver_searcher \
        tmux \
        unzip \
        w3m \
        xtools \
        zip
}

_run_system() {
    printe_h2 "Installing system packages..."

    xbps_install \
        cronie \
        iptables-nft \

    xbps_alternative iptables iptables-nft
}

_run_desktop() {
    printe_h2 "Installing desktop packages..."

    # Conflict with emacs
    if xbps_installed emacs; then
        run_root xbps-remove -Ry emacs
    fi

    # Firefox is installed with Flatpak
    xbps_install \
        emacs-x11 \
        qemu

    sh "$BASE_DIR/var/dotfiles/packages/sys/fonts.sh" "$@"
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
        pkg-config \
        postgresql-client \
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

    sh "$BASE_DIR/var/dotfiles/packages/lang/rust.sh" "$@"
    sh "$BASE_DIR/var/dotfiles/packages/lang/nim.sh" "$@"
    sh "$BASE_DIR/var/dotfiles/packages/dev/erlang.sh" "$@"
    sh "$BASE_DIR/var/dotfiles/packages/dev/elixir.sh" "$@"
    sh "$BASE_DIR/var/dotfiles/packages/dev/golang.sh" "$@"
    sh "$BASE_DIR/var/dotfiles/packages/dev/haskell.sh" "$@"
    sh "$BASE_DIR/var/dotfiles/packages/dev/node.sh" "$@"
    sh "$BASE_DIR/var/dotfiles/packages/dev/python.sh" "$@"
    sh "$BASE_DIR/var/dotfiles/packages/net/cloudflared.sh" "$@"
    sh "$BASE_DIR/var/dotfiles/packages/net/kubernetes.sh" "$@"
}

_run_all() {
    printe_h2 "Installing extra packages..."

    # Only install emacs-nox when other variant of Emacs hasn't been
    # installed (e.g. desktop flavor installs GTK emacs)
    if ! xbps_installed emacs-x11; then
        xbps_install emacs
    fi
}
