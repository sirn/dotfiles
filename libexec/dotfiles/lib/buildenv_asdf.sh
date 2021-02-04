#!/bin/sh -e
#
# Sets up build environment for asdf
#

PLATFORM=$(uname | tr '[:upper:]' '[:lower:]')
ASDF_DIR=$HOME/.asdf; export ASDF_DIR

PATH=$ASDF_DIR/bin:$ASDF_DIR/shims:$PATH; export PATH

if command -v cc >/dev/null; then
    CC=cc; export CC
    CXX=c++; export CXX
fi


## Asdf management
##

asdf_plugin() {
    plugin=$1; shift
    repo=$1; shift
    ref=$1

    git_clone "$repo" "$ASDF_DIR/plugins/$plugin" "$ref"
}

asdf_install() {
    plugin=$1; shift
    version=$1; shift

    if [ ! -d "$ASDF_DIR/installs/$plugin/$version" ]; then
        if [ "$(command -v "_asdf_install_${plugin}_$PLATFORM")x" != "x" ]; then
            "_asdf_install_${plugin}_$PLATFORM" "$plugin" "$version"
        elif [ "$(command -v "_asdf_install_$plugin")x" != "x" ]; then
            "_asdf_install_$plugin" "$plugin" "$version"
        else
            "$ASDF_DIR"/bin/asdf install "$plugin" "$version"
        fi
    fi

    if has_args "global" "$*"; then
         "$ASDF_DIR"/bin/asdf global "$plugin" "$version"
    fi

    asdf_reshim "$plugin"
}

asdf_pkg() {
    plugin=$1; shift

    if [ "$(command -v "_pkginst_${plugin}_$PLATFORM")x" != "x" ]; then
        "_asdf_pkg_${plugin}_$PLATFORM" "$@"
    elif [ "$(command -v "_pkginst_$plugin")x" != "x" ]; then
        "_asdf_pkg_$plugin" "$@"
    else
        "$@"
    fi
}

asdf_exec() {
    bin=$1; shift

    binpath="$ASDF_DIR"/shims/"$bin"
    if [ ! -f "$binpath" ]; then
        printe_info "$binpath not exists, skipping..."
        return
    fi

    "$binpath" "$@"
}

asdf_reshim() {
    plugin=$1; shift
    "$ASDF_DIR"/bin/asdf reshim "$plugin"
}
