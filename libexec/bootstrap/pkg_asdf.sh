#!/bin/sh -e
#
# Install language runtime and its packages with asdf version manager.
#

BOOTSTRAP_ROOT=${BOOTSTRAP_ROOT:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}
LOOKUP_ROOT=${LOOKUP_ROOT:-$BOOTSTRAP_ROOT}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$BOOTSTRAP_ROOT/share/bootstrap/funcs.sh"

ensure_paths required

FLAVORS=$*
PLATFORM=$(uname | tr '[:upper:]' '[:lower:]')
BUILD_DIR=$(make_temp)

ASDF_DIR=$HOME/.asdf
ASDF_SPEC=$LOOKUP_ROOT/var/bootstrap/asdf.txt


## Environment variables
##

PATH=$ASDF_DIR/bin:$ASDF_DIR/shims:$PATH; export PATH

if command -v cc >/dev/null; then
    CC=cc; export CC
    CXX=c++; export CXX
fi


## Utils
##

_do_plugin() {
    plugin=$1; shift
    repo=$1; shift

    git_clone_update "$repo" "$ASDF_DIR/plugins/$plugin"
}

_do_install() {
    plugin=$1; shift
    version=$1; shift

    if [ ! -d "$ASDF_DIR/installs/$plugin/$version" ]; then
        install=_install
        custom_install=_install_$plugin
        platform_install=_install_${plugin}_$PLATFORM

        if [ "$(command -v "$platform_install")x" != "x" ]; then
            printe_info "Running $PLATFORM installation script for $plugin..."
            install=$platform_install
        elif [ "$(command -v "$custom_install")x" != "x" ]; then
            printe_info "Running custom installation script for $plugin..."
            install=$custom_install
        fi

        $install "$plugin" "$version"
    fi

    if has_args "global" "$*"; then
         asdf global "$plugin" "$version"
    fi

    asdf reshim "$plugin"
}

_do_pkginst() {
    plugin=$1; shift
    instcmd=$*; shift

    pkglist=$LOOKUP_ROOT/var/bootstrap/pkglist_$plugin.txt

    pkginst=_pkginst
    custom_pkginst=_pkginst_$plugin
    PLATFORM_pkginst=_pkginst_${plugin}_$PLATFORM

    # shellcheck disable=SC2086
    for f in $(mangle_file $pkglist $PLATFORM "$FLAVORS"); do
        printe_h2 "Installing $plugin packages from $f..."

        if [ "$(command -v "$PLATFORM_pkginst")x" != "x" ]; then
            printe_info "Running $PLATFORM pkginst script for $plugin..."
            pkginst=$PLATFORM_pkginst
        elif [ "$(command -v "$custom_pkginst")x" != "x" ]; then
            printe_info "Running custom pkginst script for $plugin..."
            pkginst=$custom_pkginst
        fi

        $pkginst "$instcmd" "$f"
    done
}


## Installers
##

_install() {
    plugin=$1; shift
    version=$1; shift

    asdf install "$plugin" "$version"
}

_install_erlang_openbsd() {
    plugin=$1; shift
    version=$1; shift

    require_bin autoconf

    env \
        AUTOCONF_VERSION=2.69 \
        asdf install "$plugin" "$version"
}

_install_python_darwin() {
    plugin=$1; shift
    version=$1; shift

    if [ ! -d /opt/local/lib/libz.dylib ]; then
        printe_err "Building python on darwin requires sqlite3"
        printe_err "Try \`port install sqlite3\`"
        exit 1
    fi

    if [ ! -d /opt/local/lib/libsqlite3.dylib ]; then
        printe_err "Building python on darwin requires zlib"
        printe_err "Try \`port install zlib\`"
        exit 1
    fi

    # See https://github.com/pyenv/pyenv/issues/1219
    env \
        LDFLAGS="-L/opt/local/lib" \
        CPPFLAGS="-I/opt/local/include" \
        asdf install "$plugin" "$version"
}


## Package installs
##

_pkginst() {
    pkginst=$1; shift
    filename=$1; shift

    # shellcheck disable=SC2086
    xargs $pkginst < "$filename"
}

# Workaround for SQLint requiring gtar
# See https://github.com/lfittl/pg_query/pull/134
_pkginst_ruby_openbsd() {
    pkginst=$1; shift
    filename=$1; shift

    require_bin gtar "Try \`pkg_add gtar\`"

    mkdir -p "$BUILD_DIR/gnuisms"
    ln -s /usr/local/bin/gtar "$BUILD_DIR/gnuisms/tar"

    # shellcheck disable=SC2086
    env PATH="$BUILD_DIR/gnuisms:$PATH" \
        xargs $pkginst < "$filename"
}


## Installs
##

_run() {
    printe_h2 "Installing asdf..."

    git_clone_update https://github.com/asdf-vm/asdf.git "$ASDF_DIR"
    for f in $(mangle_file "$ASDF_SPEC" "$PLATFORM" "$FLAVORS"); do
        printe_h2 "Installing asdf packages from $f..."

        while read -r line; do
            case $line in
                "#"* | "" ) continue;;
                *) line="${line%%#*}";;
            esac

            eval set -- "$line"

            case $1 in
                plugin )  shift; _do_plugin  "$@";;
                install ) shift; _do_install "$@";;
                pkginst ) shift; _do_pkginst "$@";;
                * ) printe_err "Unknown directive: $1";;
            esac
        done < "$f"
    done
}

run_with_flavors "$FLAVORS"
