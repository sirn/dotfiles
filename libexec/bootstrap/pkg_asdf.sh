#!/bin/sh -e
#
# Install language runtime and its packages with asdf version manager.
#

root_dir=${BOOTSTRAP_ROOT:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}
lookup_dir=${LOOKUP_ROOT:-$root_dir}
flavors=$*

platform=$(uname | tr '[:upper:]' '[:lower:]')
asdf_dir=$HOME/.asdf

# shellcheck source=../../share/bootstrap/funcs.sh
. "$root_dir/share/bootstrap/funcs.sh"


## Tmp
##

build_dir=$(mktemp -d)
if ! normalize_bool "$NO_CLEAN_BUILDDIR"; then
    trap 'rm -rf $build_dir' 0 1 2 3 6 14 15
fi


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

    if has_args "global" "$*"; then
         asdf global "$plugin" "$version"
    fi

    asdf reshim "$plugin"
}

_do_pkginst() {
    plugin=$1; shift
    instcmd=$*; shift

    pkglist=$lookup_dir/var/bootstrap/pkglist_$plugin.txt

    pkginst=_pkginst
    custom_pkginst=_pkginst_$plugin
    platform_pkginst=_pkginst_${plugin}_$platform

    # shellcheck disable=SC2086
    for f in $(mangle_file $pkglist $platform "$flavors"); do
        printe_h2 "Installing $plugin packages from ${f##$lookup_dir/}..."

        if [ "$(command -v "$platform_pkginst")x" != "x" ]; then
            printe_info "Running $platform pkginst script for $plugin..."
            pkginst=$platform_pkginst
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

    if [ ! -d /usr/local/opt/sqlite3 ]; then
        printe_err "Building python on Darwin requires sqlite3"
        printe_err "Try \`brew install sqlite3\`"
        exit 1
    fi

    if [ ! -d /usr/local/opt/zlib ]; then
        printe_err "Building $what on Darwin requires zlib"
        printe_err "Try \`brew install zlib\`"
        exit 1
    fi

    # See https://github.com/pyenv/pyenv/issues/1219
    env \
        LDFLAGS="-L/usr/local/opt/zlib/lib -L/usr/local/opt/sqlite3/lib" \
        CPPFLAGS="-I/usr/local/opt/zlib/include -I/usr/local/opt/sqlite3/include" \
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

    mkdir -p "$build_dir/gnuisms"
    ln -s /usr/local/bin/gtar "$build_dir/gnuisms/tar"

    # shellcheck disable=SC2086
    env PATH="$build_dir/gnuisms:$PATH" \
        xargs $pkginst < "$filename"
}


## Installs
##

printe_h2 "Installing asdf..."

git_clone_update https://github.com/asdf-vm/asdf.git "$asdf_dir"

asdf_spec=$lookup_dir/var/bootstrap/asdf.txt

for f in $(mangle_file "$asdf_spec" "$platform" "$flavors"); do
    printe_h2 "Installing asdf packages from ${f##$lookup_dir/}..."

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
