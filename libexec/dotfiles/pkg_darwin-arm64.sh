#!/bin/sh -e
#
# Install Darwin packages with MacPorts and MAS.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "lib/utils.sh"
. "lib/utils_darwin.sh"
. "lib/buildenv.sh"

_run() {
    macports_bootstrap

    macports_install aria2 +sqlite3
    macports_install aspell
    macports_install aspell-dict-en
    macports_install autossh
    macports_install curl +ssl +http2
    macports_install duplicity
    macports_install emacs
    macports_install entr
    macports_install execline
    macports_install ffmpeg
    macports_install fzf
    macports_install git
    macports_install git-crypt
    macports_install git-lfs
    macports_install graphviz
    macports_install imagemagick
    macports_install ipcalc
    macports_install lima
    macports_install mercurial
    macports_install mosh
    macports_install oksh
    macports_install openssh
    macports_install pandoc
    macports_install podman
    macports_install proselint
    macports_install pstree
    macports_install qemu +target_arm +target_riscv32 +target_riscv64 +target_x86_64
    macports_install rsync
    macports_install s6
    macports_install snooze
    macports_install socat
    macports_install the_silver_searcher
    macports_install tmux
    macports_install tree
    macports_install vim
    macports_install w3m
    macports_install xcodes
    macports_install xz
    macports_install youtube-dl

    ## Broken
    #macports_install unison -gtk
}

_run_desktop() {
    macports_install emacs-mac-app
}

_run_dev() {
    macports_install carthage
    macports_install jq
    macports_install nomad
    macports_install shellcheck
    macports_install tcl
    macports_install terraform-0.14
    macports_install jsonnet

    macports_install bzip2
    macports_install libffi
    macports_install readline
    macports_install sqlite3
    macports_install zlib

    # official binary is glibc only and compiling golang from source requires
    # go-1.4 to bootstrap. Use distro package to avoid some headaches.
    macports_install go

    # asdf is required for asdf-managed packages/; install it first
    sh "$BASE_DIR/libexec/packages/sys/asdf.sh" "$@"
    sh "$BASE_DIR/libexec/packages/lang/erlang.sh" "$@"
    sh "$BASE_DIR/libexec/packages/lang/elixir.sh" "$@"
    sh "$BASE_DIR/libexec/packages/lang/nim.sh" "$@"
    sh "$BASE_DIR/libexec/packages/lang/nodejs.sh" "$@"
    sh "$BASE_DIR/libexec/packages/lang/python.sh" "$@"
    sh "$BASE_DIR/libexec/packages/lang/ruby.sh" "$@"
    sh "$BASE_DIR/libexec/packages/lang/rust.sh" "$@"
    sh "$BASE_DIR/libexec/packages/net/postgres.sh" "$@"

    # Depends on go
    sh "$BASE_DIR/libexec/packages/dev/golang.sh" "$@"
    sh "$BASE_DIR/libexec/packages/net/gcloud.sh" "$@"
    sh "$BASE_DIR/libexec/packages/net/helm.sh" "$@"
    sh "$BASE_DIR/libexec/packages/net/kubectx.sh" "$@"
    sh "$BASE_DIR/libexec/packages/net/kustomize.sh" "$@"
    sh "$BASE_DIR/libexec/packages/dev/buf.sh" "$@"
    sh "$BASE_DIR/libexec/packages/dev/parinfer-rust.sh" "$@"

    ## Requires Rosetta (no official release)
    arch -arch x86_64 sh "$BASE_DIR/libexec/packages/net/helmfile.sh" "$@"

    macports_select terraform terraform0.14
}
