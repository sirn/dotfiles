#!/bin/sh -e
#
# Install ruby packages
#

BASE_DIR=${BASE_DIR:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$BASE_DIR/share/bootstrap/funcs.sh"

GEM=

if [ -z "$BUILD_DIR" ]; then
    BUILD_DIR=$(mktemp -d)
    trap 'rm -rf $BUILD_DIR' 0 1 2 3 6 14 15
fi

_run() {
    for ver in 27 26 ""; do
        if command -v gem$ver >/dev/null; then
           GEM=gem$ver
           break
        fi
    done

    if [ -z "$GEM" ]; then
       printe_h2 "Installing ruby packages..."
       printe_info "rubygem is not installed, skipping..."
       return 1
    fi
}

_run_dev() {
    printe_h2 "Installing ruby dev packages..."
    require_bin gtar "Try \`pkg_add gtar\`"

    mkdir -p "$BUILD_DIR/gnuisms"
    ln -s /usr/local/bin/gtar "$BUILD_DIR/gnuisms/tar"
    env PATH="$BUILD_DIR/gnuisms:$PATH" \
        $GEM install --user-install --no-document \
        sqlint
}

run_with_flavors "$@"
