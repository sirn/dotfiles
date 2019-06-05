#!/bin/sh -e
#
# Install Darwin packages with Brew and MAS.
#

BOOTSTRAP_ROOT=${BOOTSTRAP_ROOT:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}
LOOKUP_ROOT=${LOOKUP_ROOT:-$BOOTSTRAP_ROOT}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$BOOTSTRAP_ROOT/share/bootstrap/funcs.sh"

ensure_paths required
ensure_platform "Darwin"

FLAVORS=$*
BREW_DIR=/usr/local/Homebrew
BREW_PKGLIST=$LOOKUP_ROOT/var/bootstrap/darwin/pkglist.txt


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
    if [ -d "$BREW_DIR/Library/Taps/$repo" ]; then
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

_setup_env() {
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
}


## Installs
##

_run() {
    _setup_env

    for f in $(mangle_file "$BREW_PKGLIST" none "$FLAVORS"); do
        printe_h2 "Installing packages from $f..."

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

    if [ "$BOOTSTRAP_ROOT" = "$LOOKUP_ROOT" ]; then
        "$BOOTSTRAP_ROOT/libexec/bootstrap/pkg_asdf.sh" "$FLAVORS"
        "$BOOTSTRAP_ROOT/libexec/bootstrap/pkg_local.sh" "$FLAVORS"
    fi
}

run_with_flavors "$FLAVORS"
