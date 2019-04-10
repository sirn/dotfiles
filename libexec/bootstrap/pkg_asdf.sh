#!/bin/sh -e
#
# Install language runtime and its packages with asdf version manager.
#

base_dir=$(cd "$(dirname "$0")/" || exit; pwd -P)
platform=$(uname | tr '[:upper:]' '[:lower:]')
flavors=$*
asdf_dir=$HOME/.asdf

cd "$base_dir" || exit 1
. ../../share/bootstrap/funcs.sh


## Environment variables
##

PATH=$asdf_dir/bin:$asdf_dir/shims:$PATH; export PATH

if command -v cc >/dev/null; then
    CC=cc; export CC
    CXX=c++; export CXX
fi


## Utils
##

_do_plugin() {
    plugin=$1; shift
    repo=$1; shift

    git_clone_update "$repo" "$asdf_dir/plugins/$plugin"
}

_do_install() {
    plugin=$1; shift
    version=$1; shift

    if [ ! -d "$asdf_dir/installs/$plugin/$version" ]; then
        install=_install
        custom_install=_install_$plugin
        platform_install=_install_${plugin}_$platform

        if [ "$(command -v "$platform_install")x" != "x" ]; then
            printe_info "Running $platform installation script for $plugin..."
            install=$platform_install
        elif [ "$(command -v "$custom_install")x" != "x" ]; then
            printe_info "Running custom installation script for $plugin..."
            install=$custom_install
        fi

        $install "$plugin" "$version"
    fi

    if [ "$(has_args "global" "$*")" = "1" ]; then
         asdf global "$plugin" "$version"
    fi

    asdf reshim "$plugin"
}

_do_pkginst() {
    plugin=$1; shift
    instcmd=$*; shift

    pkglist=../../var/bootstrap/pkglist_$plugin.txt

    # shellcheck disable=SC2086
    for f in $(mangle_file $pkglist $platform "$flavors"); do
        printe_h2 "Installing $plugin packages from ${f##../../}..."
        xargs $instcmd < "$f"
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

    if ! command -v autoconf-2.69 >/dev/null; then
        printe_err "Building erlang on OpenBSD requires autoconf 2.69"
        printe_err "Try \`pkg_add autoconf%2.69\`"
    fi

    env \
        AUTOCONF_VERSION=2.69 \
        asdf install "$plugin" "$version"
}

_install_python_darwin() {
    plugin=$1; shift
    version=$1; shift

    if [ ! -d /usr/local/opt/zlib ]; then
        printe_err "Building python on Darwin requires zlib"
        printe_err "Try \`brew install zlib\`"
        exit 1
    fi

    if [ ! -d /usr/local/opt/sqlite3 ]; then
        printe_err "Building python on Darwin requires sqlite3"
        printe_err "Try \`brew install sqlite3\`"
        exit 1
    fi

    # See https://github.com/pyenv/pyenv/issues/1219
    env \
        LDFLAGS="-L/usr/local/opt/zlib/lib -L/usr/local/opt/sqlite3/lib" \
        CPPFLAGS="-I/usr/local/opt/zlib/include -I/usr/local/opt/sqlite3/include" \
        asdf install "$plugin" "$version"
}


## Installs
##

printe_h2 "Installing asdf..."

git_clone_update https://github.com/asdf-vm/asdf.git "$asdf_dir"

asdf_spec=../../var/bootstrap/asdf.txt

for f in $(mangle_file "$asdf_spec" "$platform" "$flavors"); do
    printe_h2 "Installing asdf packages from ${f##../../}..."

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
