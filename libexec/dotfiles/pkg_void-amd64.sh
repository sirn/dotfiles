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

    xbps_install GraphicsMagick
    xbps_install aria2
    xbps_install aspell
    xbps_install aspell-en
    xbps_install curl
    xbps_install duplicity
    xbps_install entr
    xbps_install execline
    xbps_install fuse-overlayfs
    xbps_install fzf
    xbps_install git
    xbps_install git-crypt
    xbps_install git-lfs
    xbps_install graphviz
    xbps_install mdBook
    xbps_install mercurial
    xbps_install mosh
    xbps_install ipcalc
    xbps_install neovim
    xbps_install oksh
    xbps_install podman
    xbps_install podman-compose
    xbps_install python3-proselint
    xbps_install python3-tmuxp
    xbps_install rsync
    xbps_install s6
    xbps_install snooze
    xbps_install socat
    xbps_install sqlite
    xbps_install the_silver_searcher
    xbps_install tmux
    xbps_install tree
    xbps_install unison
    xbps_install unzip
    xbps_install w3m
    xbps_install xtools
    xbps_install xz
    xbps_install zip
}

_run_system() {
    printe_h2 "Installing system packages..."

    xbps_install cronie
    xbps_install iptables-nft

    xbps_alternative iptables iptables-nft
}

_run_desktop() {
    printe_h2 "Installing desktop packages..."

    # Conflict with emacs
    if xbps_installed emacs; then
        run_root xbps-remove -Ry emacs
    fi

    # Firefox is installed with Flatpak
    xbps_install emacs-x11
    xbps_install qemu

    sh "$BASE_DIR/libexec/packages/sys/fonts.sh" "$@"
}

_run_dev() {
    printe_h2 "Installing dev packages..."

    xbps_install ansible
    xbps_install jq
    xbps_install jsonnet
    xbps_install nomad
    xbps_install pandoc
    xbps_install shellcheck
    xbps_install tcl
    xbps_install terraform

    xbps_install base-devel
    xbps_install bzip2-devel
    xbps_install libffi-devel
    xbps_install readline-devel
    xbps_install sqlite-devel
    xbps_install zlib-devel

    # official binary is glibc only and compiling golang from source requires
    # go-1.4 to bootstrap. Use distro package to avoid some headaches.
    xbps_install go

    # asdf is required for asdf-managed packages/; install it first
    sh "$BASE_DIR/libexec/packages/sys/asdf.sh" "$@"
    sh "$BASE_DIR/libexec/packages/lang/nim.sh" "$@"
    sh "$BASE_DIR/libexec/packages/lang/nodejs.sh" "$@"
    sh "$BASE_DIR/libexec/packages/lang/python.sh" "$@"
    sh "$BASE_DIR/libexec/packages/lang/rust.sh" "$@"
    sh "$BASE_DIR/libexec/packages/lang/erlang.sh" "$@"
    sh "$BASE_DIR/libexec/packages/lang/elixir.sh" "$@"
    sh "$BASE_DIR/libexec/packages/net/postgres.sh" "$@"

    # Usually requires language interpreter to be installed
    sh "$BASE_DIR/libexec/packages/dev/golang.sh" "$@"
    sh "$BASE_DIR/libexec/packages/net/gcloud.sh" "$@"
    sh "$BASE_DIR/libexec/packages/net/kubectx.sh" "$@"
    sh "$BASE_DIR/libexec/packages/net/helm.sh" "$@"
}

_run_all() {
    printe_h2 "Installing extra packages..."

    # Only install emacs-nox when other variant of Emacs hasn't been
    # installed (e.g. desktop flavor installs GTK emacs)
    if ! xbps_installed emacs-x11; then
        xbps_install emacs
    fi
}
