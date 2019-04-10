#!/bin/sh -e
#
# Create links per the given spec.
#

base_dir=$(cd "$(dirname "$0")/" || exit; pwd -P)
platform=$(uname | tr '[:upper:]' '[:lower:]')
flavors=$*

cd "$base_dir" || exit 1
. ../../share/bootstrap/funcs.sh


## Utils
##

_make_link() {
    src=$1; shift
    dest=$1; shift

    case $src in
        /* ) printe "$src cannot be an absolute path"; return;;
        * ) src=$(cd "$(dirname "$base_dir/../../../")" || exit; pwd -P)/$src;;
    esac

    make_link "$src" "$dest"
}


## Links
##

linklist="../../var/bootstrap/links.txt"

for f in $(mangle_file "$linklist" "$platform" "$flavors"); do
    printe_h2 "Linking files in ${f##../../}..."

    while read -r line; do
        case $line in
            "#"* | "" ) continue;;
            *) line=${line%%#*};;
        esac

        eval set -- "$line"

        _make_link "$@"
    done < "$f"
done
