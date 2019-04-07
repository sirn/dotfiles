#!/bin/sh -e
#
# Install Darwin packages with Brew and MAS.
#

base_dir=$(cd "$(dirname "$0")/" || exit; pwd -P)
brew_dir="/usr/local/Homebrew"
flavors=$*

cd "$base_dir" || exit 1
. ../../share/bootstrap/funcs.sh
. ../../share/bootstrap/compat.sh

if [ "$(uname)" != "Darwin" ]; then
    printe_err "Not a Darwin system"
    exit 1
fi


## Utils
##

_brew_env() {
    env \
        HOMEBREW_NO_EMOJI=1 \
        HOMEBREW_NO_ANALYTICS=1 \
        HOMEBREW_NO_COLOR=1 \
        "$@"
}

_do_tap() {
    tap=$1; shift

    repo="$(printf "%s" "$tap" | sed 's|\(.*\)/\([^/]*\)$|\1/homebrew-\2|')"
    if [ -d "$brew_dir/Library/Taps/$repo" ]; then
        printe "$tap already tapped"
        return
    fi

    _brew_env brew tap "$tap"
}

_do_install() {
    pkg=$1; shift

    if [ -d "/usr/local/Cellar/$pkg" ]; then
        printe "$pkg already installed"
        return
    fi

    printe "Installing $pkg..."
    _brew_env brew install "$pkg"
}

_do_cask_install() {
    pkg=$1; shift

    if [ -d "/usr/local/Caskroom/$pkg" ]; then
        printe "$pkg (cask) already installed"
        return
    fi

    printe "Installing $pkg..."
    _brew_env brew cask install "$pkg"
}

_do_mas_install() {
    pkg_id=$1; shift
    app_name=$*

    if [ -d "/Applications/${app_name}.app" ]; then
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

    sdk="/Library/Developer/CommandLineTools/Packages/macOS_SDK_headers_for_macOS_10.14.pkg"
    if [ -e "$sdk" ] && [ ! -f /usr/lib/bundle1.o ]; then
        run_root /usr/sbin/installer -pkg "$sdk" -target /
    fi

    fetch_url - https://raw.githubusercontent.com/Homebrew/install/master/install | /usr/bin/ruby
fi

printe_h2 "Installing runtime requisites..."

_do_cask_install java
_do_install mas


## Installs
##

pkglist="../../var/bootstrap/darwin/pkglist.txt"

for f in $(mangle_file "$pkglist" none "$flavors"); do
    printe_h2 "Installing packages from ${f##../../}..."

    while read -r spec; do
        case "$spec" in
            "#"* | "" ) continue;;
            *) spec="${spec%%#*}";;
        esac

        eval set -- "$spec"

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

"$base_dir/pkg_asdf.sh" "$flavors"
"$base_dir/pkg_local.sh" "$flavors"
