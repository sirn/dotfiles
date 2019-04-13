#!/bin/sh -e
#
# Create links per the given spec.
#

root_dir=${BOOTSTRAP_ROOT:-../../}
lookup_dir=${LOOKUP_ROOT:-$root_dir}
flavors=$*

platform=$(uname | tr '[:upper:]' '[:lower:]')

# shellcheck source=../../share/bootstrap/funcs.sh
. "$root_dir/share/bootstrap/funcs.sh"


## Utils
##

_make_link() {
    src=$1; shift
    dest=$1; shift

    case $src in
        /* ) printe "$src cannot be an absolute path"; return;;
        * ) src=$lookup_dir/$src;;
    esac

    make_link "$src" "$dest"
}


## Links
##

linklist="$lookup_dir/var/bootstrap/links.txt"

for f in $(mangle_file "$linklist" "$platform" "$flavors"); do
    printe_h2 "Linking files in ${f##$lookup_dir/}..."

    while read -r line; do
        case $line in
            "#"* | "" ) continue;;
            *) line=${line%%#*};;
        esac

        eval set -- "$line"

        _make_link "$@"
    done < "$f"
done
