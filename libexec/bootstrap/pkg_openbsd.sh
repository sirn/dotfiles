#!/bin/sh -e
#
# Install OpenBSD packages with Pkg.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../share/bootstrap/utils.sh"
. "../../share/bootstrap/utils_openbsd.sh"

_run() {
    printe_h2 "Installing packages..."

    pkg_install aria2
    pkg_install aspell
    pkg_install base64
    pkg_install colorls
    pkg_install coreutils
    pkg_install curl
    pkg_install gdiff
    pkg_install git
    pkg_install gtar--static
    pkg_install jdk%1.8
    pkg_install mercurial
    pkg_install mosh
    pkg_install pstree
    pkg_install socat
    pkg_install the_silver_searcher
    pkg_install unzip--iconv
    pkg_install weechat
    pkg_install weechat-python
}

_run_desktop() {
    printe_h2 "Installing desktop packages..."

    pkg_install emacs--gtk3
    pkg_install firefox

    sh "$BASE_DIR/libexec/packages/fontinst.sh" "$@"
}

_run_dev() {
    printe_h2 "Installing dev packages..."

    pkg_install GraphicsMagick
    pkg_install ansible
    pkg_install autoconf%2.69
    pkg_install automake%1.16
    pkg_install cabal-install
    pkg_install doxygen
    pkg_install duplicity
    pkg_install elixir
    pkg_install entr
    pkg_install erlang%21
    pkg_install expect
    pkg_install ghc
    pkg_install git-lfs
    pkg_install go
    pkg_install google-cloud-sdk
    pkg_install graphviz
    pkg_install ipcalc
    pkg_install jq
    pkg_install metaauto
    pkg_install node
    pkg_install pkgconf
    pkg_install py3-pip
    pkg_install python%3
    pkg_install ruby%2.6
    pkg_install rust
    pkg_install socat
    pkg_install terraform
    pkg_install tree

    printe_info "Installing default ruby links..."

    run_root ln -sf /usr/local/bin/bundle26 /usr/local/bin/bundle
    run_root ln -sf /usr/local/bin/bundler26 /usr/local/bin/bundler
    run_root ln -sf /usr/local/bin/erb26 /usr/local/bin/erb
    run_root ln -sf /usr/local/bin/gem26 /usr/local/bin/gem
    run_root ln -sf /usr/local/bin/irb26 /usr/local/bin/irb
    run_root ln -sf /usr/local/bin/rake26 /usr/local/bin/rake
    run_root ln -sf /usr/local/bin/rdoc26 /usr/local/bin/rdoc
    run_root ln -sf /usr/local/bin/ri26 /usr/local/bin/ri
    run_root ln -sf /usr/local/bin/ruby26 /usr/local/bin/ruby

    printe_info "Installing default erlang links..."

    run_root ln -sf /usr/local/bin/ct_run21 /usr/local/bin/ct_run
    run_root ln -sf /usr/local/bin/dialyzer21 /usr/local/bin/dialyzer
    run_root ln -sf /usr/local/bin/epmd21 /usr/local/bin/epmd
    run_root ln -sf /usr/local/bin/erl21 /usr/local/bin/erl
    run_root ln -sf /usr/local/bin/erl_call21 /usr/local/bin/erl_call
    run_root ln -sf /usr/local/bin/erlc21 /usr/local/bin/erlc
    run_root ln -sf /usr/local/bin/escript21 /usr/local/bin/escript
    run_root ln -sf /usr/local/bin/run_erl21 /usr/local/bin/run_erl
    run_root ln -sf /usr/local/bin/to_erl21 /usr/local/bin/to_erl
    run_root ln -sf /usr/local/bin/typer21 /usr/local/bin/typer

    printe_info "Installing default python links..."

    run_root ln -sf /usr/local/bin/easy_install-3.6 /usr/local/bin/easy_install
    run_root ln -sf /usr/local/bin/pip3.6 /usr/local/bin/pip3
    run_root ln -sf /usr/local/bin/pyvenv-3.6 /usr/local/bin/pyvenv

    sh "$BASE_DIR/libexec/packages/cloudflared.sh" "$@"
    sh "$BASE_DIR/libexec/packages/execline.sh" "$@"
    sh "$BASE_DIR/libexec/packages/git-crypt.sh" "$@"
    sh "$BASE_DIR/libexec/packages/haskell.sh" "$@"
    sh "$BASE_DIR/libexec/packages/leiningen.sh" "$@"
    sh "$BASE_DIR/libexec/packages/node.sh" "$@"
    sh "$BASE_DIR/libexec/packages/python.sh" "$@"
    sh "$BASE_DIR/libexec/packages/rebar3.sh" "$@"
    sh "$BASE_DIR/libexec/packages/rust.sh" "$@"

    # Restore Google Cloud state directory. This is required for kubectl
    # to be able to authenticate with Google Cloud.
    if [ -d /usr/local/google-cloud-sdk ]; then
        run_root mkdir -p /usr/local/google-cloud-sdk/.install
    fi
}

_run_kubernetes() {
    printe_h2 "Installing kubernetes packages..."
    sh "$BASE_DIR/libexec/packages/kubernetes-cli.sh" "$@"
    sh "$BASE_DIR/libexec/packages/kubernetes-helm.sh" "$@"
    sh "$BASE_DIR/libexec/packages/kubectx.sh" "$@"
    sh "$BASE_DIR/libexec/packages/kapitan.sh" "$@"
}

_run_all() {
    run_with_flavors "$@"

    # Only install emacs--no_x11 when other variant of Emacs hasn't been
    # installed (e.g. desktop flavor installs emacs--gtk3)
    if ! pkg_installed emacs; then
        pkg_install emacs--no_x11
    fi

    # Only install w3m-- when other variant of w3m hasn't been installed
    # (e.g. desktop flavor installs w3m--image which is required by emacs-w3m
    # when running under GUI mode)
    if ! pkg_installed w3m; then
        pkg_install w3m--
    fi
}

_run_all "$@"
