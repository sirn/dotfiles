#!/bin/sh -e
#
# Install Darwin packages with MacPorts and MAS.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../share/bootstrap/utils.sh"
. "../../share/bootstrap/utils_darwin.sh"
. "../../share/bootstrap/buildenv.sh"

_run() {
    printe_h2 "Installing packages..."

    macports_bootstrap

    macports_install aria2 +sqlite3
    macports_install aspell
    macports_install aspell-dict-en
    macports_install curl +darwinssl +http2
    macports_install dnscrypt-proxy
    macports_install emacs
    macports_install git
    macports_install ipfs
    macports_install mercurial
    macports_install mosh
    macports_install oksh
    macports_install openssh
    macports_install pstree
    macports_install py37-tmuxp
    macports_install qemu +target_arm +target_riscv64
    macports_install socat
    macports_install the_silver_searcher
    macports_install tmux
    macports_install w3m
    macports_install weechat +python36 +aspell
}

_run_desktop() {
    printe_h2 "Installing desktop packages..."

    macports_install emacs-mac-app
    macports_install mpv

    mas_install 407963104 Pixelmator
    mas_install 411643860 DaisyDisk
    mas_install 413965349 Soulver
    mas_install 497799835 Xcode
    mas_install 603637384 Name Mangler 3
    mas_install 775737590 iA Writer
    mas_install 975937182 Fantastical 2
    mas_install 1333542190 1Password 7
    mas_install 1435957248 Drafts
}

_run_dev() {
    printe_h2 "Installing dev packages..."

    macports_install GraphicsMagick
    macports_install autoconf
    macports_install carthage
    macports_install duplicity
    macports_install elixir
    macports_install entr
    macports_install erlang
    macports_install git-crypt
    macports_install git-lfs
    macports_install go
    macports_install google-cloud-sdk
    macports_install graphviz
    macports_install hs-cabal-install
    macports_install ipcalc
    macports_install jq
    macports_install leiningen
    macports_install nodejs10
    macports_install npm6
    macports_install pandoc
    macports_install py37-pip
    macports_install python37
    macports_install rebar3
    macports_install ruby26
    macports_install socat
    macports_install terraform
    macports_install tree
    macports_install xz
    macports_install zlib

    # Outdated
    # https://github.com/macports/macports-ports/pull/4577
    #macports_install shellcheck

    macports_select pip3 pip37
    macports_select python3 python37
    macports_select ruby ruby26

    sh "$BASE_DIR/libexec/packages/cloudflared.sh" "$@"
    sh "$BASE_DIR/libexec/packages/haskell.sh" "$@"
    sh "$BASE_DIR/libexec/packages/node.sh" "$@"
    sh "$BASE_DIR/libexec/packages/python.sh" "$@"
    sh "$BASE_DIR/libexec/packages/rust.sh" "$@"
}

run_with_flavors "$@"
