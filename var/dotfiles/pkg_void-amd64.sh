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
        GraphicsMagick \
        aria2 \
        aspell \
        aspell-en \
        curl \
        duplicity \
        execline \
        fzf \
        git \
        mercurial \
        mosh \
        neovim \
        oksh \
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
        tree \
        unison \
        unzip \
        w3m \
        xtools \
        xz \
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

    # Rust/Nim has its own versioning ecosystem and Nix support
    # is still very limited
    xbps_install \
        cargo \
        choosenim

    sh "$BASE_DIR/var/dotfiles/packages/lang/rust.sh" "$@"
    sh "$BASE_DIR/var/dotfiles/packages/lang/nim.sh" "$@"

    # Nix
    printe_info "\
Development packages on Void is managed by Nix.
Please ensure nix is installed by running:

    xbps-install -Syu nix
    ln -s /etc/sv/nix-daemon /var/service/

Then run the following:

    nix-channel --add http://nixos.org/channels/nixpkgs-unstable
    nix-channel --update
    nix-env -i all
"
}

_run_all() {
    printe_h2 "Installing extra packages..."

    # Only install emacs-nox when other variant of Emacs hasn't been
    # installed (e.g. desktop flavor installs GTK emacs)
    if ! xbps_installed emacs-x11; then
        xbps_install emacs
    fi
}
