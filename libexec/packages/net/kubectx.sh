#!/bin/sh -e
#
# Install Kubectx.
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../../../.." || exit; pwd -P)}

cd "$(dirname "$0")" || exit 1
. "../../dotfiles/lib/utils.sh"
. "../../dotfiles/lib/buildenv.sh"

KUBECTX_VER=0.9.1
KUBECTX_SHA256=8f68e19b841a1f1492536dc27f9b93ea3204c7e4fd0ad2e3c483d1b8e95be675

_preflight() {
    if ! command -v go >/dev/null; then
        printe_info "go is not installed, skipping..."
        return 1
    fi
}

_run() {
    _install_kubectx
}

_install_kubectx() {
    printe_h2 "Installing kubectx..."

    _verdir=$HOME/.cache/local-pkg/kubectx/$KUBECTX_VER

    if ! forced && [ -f "$_verdir/bin/kubectx" ]; then
        printe_info "$_verdir/bin/kubectx already exists, skipping..."
        return
    fi

    cd "$BUILD_DIR" || exit 1

    fetch_gh_archive kubectx.tar.gz ahmetb/kubectx v$KUBECTX_VER
    verify_shasum kubectx.tar.gz $KUBECTX_SHA256
    run_tar -C "$BUILD_DIR" -xzf kubectx.tar.gz
    rm kubectx.tar.gz

    worksrc="$BUILD_DIR/go/src/github.com/ahmetb"
    mkdir -p "$worksrc"
    mv \
        "$BUILD_DIR/kubectx-$KUBECTX_VER" \
        "$worksrc/kubectx"

    for n in kubectx kubens; do
        cd "$worksrc/kubectx/cmd/$n" || exit 1
        go install

        cd "$BUILD_DIR/go/bin" || exit 1
        install -d "$_verdir/bin"
        install -m0755 "$n" "$_verdir/bin/$n"

        # https://github.com/ahmetb/kubectx/issues/254
        cat <<EOF > "$HOME/.local/bin/$n"
#!/bin/sh
# https://github.com/ahmetb/kubectx/issues/254
XDG_CACHE_HOME=; export XDG_CACHE_HOME
exec "$_verdir/bin/$n" "\$@"
EOF

        chmod +x "$HOME/.local/bin/$n"
    done
}

run_with_flavors "$@"
