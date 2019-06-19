#!/bin/sh -e
#
# Shared functions for asdf version manager
#

PLATFORM=$(uname | tr '[:upper:]' '[:lower:]')
ASDF_DIR=$HOME/.asdf

PATH=$ASDF_DIR/bin:$ASDF_DIR/shims:$PATH; export PATH

if command -v cc >/dev/null; then
    CC=cc; export CC
    CXX=c++; export CXX
fi


## Asdf management
##

_asdf_plugin() {
    plugin=$1; shift
    repo=$1; shift
    ref=$1

    git_clone "$repo" "$ASDF_DIR/plugins/$plugin" "$ref"
}

_asdf_install() {
    plugin=$1; shift
    version=$1; shift

    if [ ! -d "$ASDF_DIR/installs/$plugin/$version" ]; then
        if [ "$(command -v "_asdf_install_${plugin}_$PLATFORM")x" != "x" ]; then
            "_asdf_install_${plugin}_$PLATFORM" "$plugin" "$version"
        elif [ "$(command -v "_asdf_install_$plugin")x" != "x" ]; then
            "_asdf_install_$plugin" "$plugin" "$version"
        else
            asdf install "$plugin" "$version"
        fi
    fi

    if has_args "global" "$*"; then
         asdf global "$plugin" "$version"
    fi

    asdf reshim "$plugin"
}

_asdf_pkg() {
    plugin=$1; shift

    if [ "$(command -v "_pkginst_${plugin}_$PLATFORM")x" != "x" ]; then
        "_asdf_pkg_${plugin}_$PLATFORM" "$@"
    elif [ "$(command -v "_pkginst_$plugin")x" != "x" ]; then
        "_asdf_pkg_$plugin" "$@"
    else
        "$@"
    fi
}


## Defs
##

_asdf_install_erlang_openbsd() {
    plugin=$1; shift
    version=$1; shift

    require_bin autoconf

    env \
        AUTOCONF_VERSION=2.69 \
        asdf install "$plugin" "$version"
}

_asdf_install_python_darwin() {
    plugin=$1; shift
    version=$1; shift

    if [ ! -f /opt/local/lib/libsqlite3.dylib ]; then
        printe_err "Building python on darwin requires sqlite3"
        printe_err "Try \`port install sqlite3\`"
        exit 1
    fi

    # See https://github.com/pyenv/pyenv/issues/1219
    env \
        LDFLAGS="-L/opt/local/lib" \
        CPPFLAGS="-I/opt/local/include" \
        asdf install "$plugin" "$version"
}

# Workaround for SQLint requiring gtar
# See https://github.com/lfittl/pg_query/pull/134
_asdf_pkg_ruby_openbsd() {
    require_bin gtar "Try \`pkg_add gtar\`"
    mkdir -p "$BUILD_DIR/gnuisms"
    ln -s /usr/local/bin/gtar "$BUILD_DIR/gnuisms/tar"
    env PATH="$BUILD_DIR/gnuisms:$PATH" "$@"
}
