#!/bin/sh -e
#
# Sets up build environment
#

if [ -z "$BUILD_DIR" ]; then
    BUILD_DIR=$(mktemp -d)

    _cleanup_build_dir() {
        rm -rf "$BUILD_DIR" >/dev/null 2>&1

        # Sometimes a permission changed during installation
        # so we need to force removal with root.
        if [ -d "$BUILD_DIR" ]; then
            run_root rm -rf "$BUILD_DIR" >/dev/null 2>&1
        fi
    }

    trap _cleanup_build_dir 0 1 2 3 6 14 15
fi

GOPATH="$BUILD_DIR/go"
PATH="$GOPATH/bin:$PATH"

export BUILD_DIR
export GOPATH
export PATH
