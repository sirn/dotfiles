#!/bin/sh -e
#
# Create links per the given spec.
#

base_dir=$(cd "$(dirname "$0")/" || exit; pwd -P)
platform=$(uname | tr '[:upper:]' '[:lower:]')
flavors=$*

cd "$base_dir" || exit 1
. ../../share/bootstrap/funcs.sh
. ../../share/bootstrap/compat.sh


## Utils
##

_make_link() {
    src=$1; shift
    dest=$1; shift

    case "$src" in
        /* ) printe "$src cannot be an absolute path"; return;;
        * ) src="$(cd "$(dirname "$base_dir/../../../")" || exit; pwd -P)/$src";;
    esac

    if [ ! -f "$src" ]; then
        printe "$src does not exists, skipping"
        return
    fi

    if [ "$(normalize_bool "$FORCE")" != "1" ] && [ -f "$dest" ] && [ ! -L "$dest" ]; then
        printe "$dest already exists and is not a link, skipping"
        return
    fi

    if [ "$(normalize_bool "$FORCE")" != "1" ] && [ "$(readlink "$dest")" = "$src" ]; then
        printe "$dest already linked"
        return
    fi

    mkdir -p "$(dirname "$dest")"
    rm -f "$dest"
    ln -s "$src" "$dest"
    printe "$dest has been linked to $src"
}

_make_links() {
    linklist=$1; shift

    if [ ! -f "$linklist" ]; then
        printe_info "${linklist##../../} could not be found, skipping"
        return
    fi

    printe_h2 "Linking files in ${linklist##../../}..."

    while read -r spec; do
        case "$spec" in
            "#"* | "" ) continue;;
            *) spec="${spec%%#*}";;
        esac

        eval set -- "$spec"

        _make_link "$@"
    done < "$linklist"
}


## Links
##

_make_links "../../var/bootstrap/links.txt"
_make_links "../../var/bootstrap/${platform}/links.txt"

for flavor in $flavors; do
    _make_links "../../var/bootstrap/links.${flavor}.txt"
    _make_links "../../var/bootstrap/${platform}/links.${flavor}.txt"
done
