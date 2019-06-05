#!/bin/sh -e
#
# Create links per the given spec.
#

BOOTSTRAP_ROOT=${BOOTSTRAP_ROOT:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}
LOOKUP_ROOT=${LOOKUP_ROOT:-$BOOTSTRAP_ROOT}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$BOOTSTRAP_ROOT/share/bootstrap/funcs.sh"

ensure_paths required

FLAVORS=$*
PLATFORM=$(uname | tr '[:upper:]' '[:lower:]')
LINKLIST="$LOOKUP_ROOT/var/bootstrap/links.txt"


## Utils
##

_make_link() {
    src=$1; shift
    dest=$1; shift

    case $src in
        /* ) printe "$src cannot be an absolute path"; return;;
        * ) src=$LOOKUP_ROOT/$src;;
    esac

    make_link "$src" "$dest"
}


## Run
##

_run() {
    for f in $(mangle_file "$LINKLIST" "$PLATFORM" "$FLAVORS"); do
        printe_h2 "Linking files in $f..."

        while read -r line; do
            case $line in
                "#"* | "" ) continue;;
                *) line=${line%%#*};;
            esac

            eval set -- "$line"

            _make_link "$@"
        done < "$f"
    done
}

run_with_flavors "$FLAVORS"
