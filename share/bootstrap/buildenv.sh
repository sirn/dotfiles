#!/bin/sh -e
#
# Sets up build environment
#

. "../../share/bootstrap/utils.sh"

if [ -z "$BUILD_DIR" ]; then
    BUILD_DIR=$(mktemp -d)

    _cleanup_build_dir() {
        run_root rm -rf "$BUILD_DIR"
    }

    trap _cleanup_build_dir 0 1 2 3 6 14 15
fi

GOPATH="$BUILD_DIR/go"
PATH="$GOPATH/bin:$PATH"

export BUILD_DIR
export GOPATH
export PATH
