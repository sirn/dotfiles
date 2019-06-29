#!/bin/sh -e
#
# Install OpenBSD packages with Pkg.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$BASE_DIR/share/bootstrap/funcs.sh"

# shellcheck source=../../share/bootstrap/openbsd.sh
. "$BASE_DIR/share/bootstrap/openbsd.sh"

_run() {
    printe_h2 "Installing packages..."
    _do_pkg aria2
    _do_pkg aspell
    _do_pkg base64
    _do_pkg colorls
    _do_pkg coreutils
    _do_pkg curl
    _do_pkg gdiff
    _do_pkg git
    _do_pkg gtar--static
    _do_pkg jdk%1.8
    _do_pkg mercurial
    _do_pkg mosh
    _do_pkg pstree
    _do_pkg socat
    _do_pkg the_silver_searcher
    _do_pkg unzip--iconv
    _do_pkg weechat
    _do_pkg weechat-python
}

_run_desktop() {
    printe_h2 "Installing desktop packages..."
    _do_pkg adobe-source-code-pro
    _do_pkg adobe-source-sans-pro
    _do_pkg adobe-source-serif-pro
    _do_pkg emacs--gtk3
    _do_pkg feh
    _do_pkg firefox
    _do_pkg noto-cjk
    _do_pkg noto-emoji
    _do_pkg noto-fonts
    _do_pkg w3m--image
}

_run_dev() {
    printe_h2 "Installing dev packages..."
    _do_pkg GraphicsMagick
    _do_pkg ansible
    _do_pkg autoconf%2.69
    _do_pkg automake%1.16
    _do_pkg cabal-install
    _do_pkg doxygen
    _do_pkg duplicity
    _do_pkg elixir
    _do_pkg entr
    _do_pkg erlang%21
    _do_pkg expect
    _do_pkg ghc
    _do_pkg git-lfs
    _do_pkg go
    _do_pkg google-cloud-sdk
    _do_pkg graphviz
    _do_pkg ipcalc
    _do_pkg jq
    _do_pkg metaauto
    _do_pkg node
    _do_pkg pkgconf
    _do_pkg py3-pip
    _do_pkg python%3
    _do_pkg ruby%2.6
    _do_pkg rust
    _do_pkg socat
    _do_pkg terraform
    _do_pkg tree

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
    if ! _check_installed emacs; then
        run_root pkg_add emacs--no_x11
    fi

    # Only install w3m-- when other variant of w3m hasn't been installed
    # (e.g. desktop flavor installs w3m--image which is required by emacs-w3m
    # when running under GUI mode)
    if ! _check_installed w3m; then
        run_root pkg_add w3m--
    fi
}

_run_all "$@"
