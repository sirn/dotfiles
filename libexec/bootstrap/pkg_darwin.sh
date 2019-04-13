#!/bin/sh -e
#
# Install Darwin packages with Brew and MAS.
#

root_dir=${BOOTSTRAP_ROOT:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}
lookup_dir=${LOOKUP_ROOT:-$root_dir}
flavors=$*

brew_dir=/usr/local/Homebrew

# shellcheck source=../../share/bootstrap/funcs.sh
. "$root_dir/share/bootstrap/funcs.sh"

if [ "$(uname)" != "Darwin" ]; then
    printe_err "Not a Darwin system"
    exit 1
fi


## Environment variables
##

HOMEBREW_NO_EMOJI=1; export HOMEBREW_NO_EMOJI
HOMEBREW_NO_ANALYTICS=1; export HOMEBREW_NO_ANALYTICS
HOMEBREW_NO_COLOR=1; export HOMEBREW_NO_COLOR


## Utils
##

_do_tap() {
    tap=$1; shift

    repo=$(printf "%s" "$tap" | sed 's|\(.*\)/\([^/]*\)$|\1/homebrew-\2|')
    if [ -d "$brew_dir/Library/Taps/$repo" ]; then
        printe "$tap already tapped"
        return
    fi

    brew tap "$tap"
}

_do_install() {
    pkg=$1; shift

    if [ -d "/usr/local/Cellar/$pkg" ]; then
        printe "$pkg already installed"
        return
    fi

    printe "Installing $pkg..."
    brew install "$pkg"
}

_do_cask_install() {
    pkg=$1; shift

    if [ -d "/usr/local/Caskroom/$pkg" ]; then
        printe "$pkg (cask) already installed"
        return
    fi

    printe "Installing $pkg..."
    brew cask install "$pkg"
}

_do_mas_install() {
    pkg_id=$1; shift
    app_name=$*

    if [ -d "/Applications/$app_name.app" ]; then
        printe "$app_name (mas) already installed"
        return
    fi

    printe "Installing $app_name (mas)..."
    mas install "$pkg_id"
}


## Setup
##

if [ ! -x /usr/local/bin/brew ]; then
    printe_h2 "Bootstrapping Homebrew..."

    xcode-select --install 2>/dev/null

    sdk=/Library/Developer/CommandLineTools/Packages/macOS_SDK_headers_for_macOS_10.14.pkg
    if [ -e $sdk ] && [ ! -f /usr/lib/bundle1.o ]; then
        run_root /usr/sbin/installer -pkg $sdk -target /
    fi

    fetch_url - https://raw.githubusercontent.com/Homebrew/install/master/install | /usr/bin/ruby
fi

printe_h2 "Installing runtime requisites..."

_do_cask_install java
_do_install mas


## Installs
##

pkglist=$lookup_dir/var/bootstrap/darwin/pkglist.txt

for f in $(mangle_file "$pkglist" none "$flavors"); do
    printe_h2 "Installing packages from ${f##$lookup_dir/}..."

    while read -r line; do
        case $line in
            "#"* | "" ) continue;;
            *) line=${line%%#*};;
        esac

        eval set -- "$line"

        case "$1" in
            tap  ) shift; _do_tap "$@";;
            brew ) shift; _do_install "$@";;
            cask ) shift; _do_cask_install "$@";;
            mas )  shift; _do_mas_install "$@";;
            * ) printe_err "Unknown directive: $1";;
        esac
    done < "$f"
done


## Hand-off
##

if [ "$root_dir" = "$lookup_dir" ]; then
    "$root_dir/libexec/bootstrap/pkg_asdf.sh" "$flavors"
    "$root_dir/libexec/bootstrap/pkg_local.sh" "$flavors"
fi
