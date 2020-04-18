#!/bin/sh
#
# Compare packages that are locally built.
#

CURDIR=${1:-$(pwd -P)}
TMP_DIR=$(mktemp -d)
trap 'rm -rf $TMP_DIR' 0 1 2 3 6 14 15

T_BOLD="$(tput bold 2>/dev/null || true)"
T_RESET="$(tput sgr0 2>/dev/null || true)"

print_help() {
    printf >&2 "Usage: %s [DIR]

Check for void-linux xbps-src package updates by comparing non-remote
installation of packages with the xbps-src source.

Arguments:

    DIR     Path to directory containing xbps-src

" "$(basename "$0")"
}

if ! command -v xbps-query >/dev/null; then
    printf >&2 "xbps-query not found, not a Void Linux system?\\n"
    exit 1
fi

if [ ! -f "$CURDIR/xbps-src" ]; then
    print_help
    exit 1
fi

mkfifo "$TMP_DIR/packages"
xbps-query -s '^/' --property repository --regex > "$TMP_DIR/packages" &

version_gte() {
    left=$1; shift
    right=$1; shift

    # https://havoc.io/post/shellsemver/
    min_ver=$(printf "%s\\n%s" "$left" "$right" |
        sort -t "." -n -k1,1 -k2,2 -k3,3 -k4,4 |
        head -n 1)

    if [ "$right" = "$min_ver" ]; then
        return 0
    fi

    return 1
}

while read -r n; do
    n=${n%: *}
    pkg=${n%-*}

    cur_pkgver=$n
    cur_version=${cur_pkgver#$pkg-}
    cur_version=${cur_version%_*}
    cur_revision=${cur_pkgver#*_}

    "$CURDIR"/xbps-src show "$pkg" > "$TMP_DIR/${pkg}_new"

    new_version=$(awk '/version:/ { print $2 }' < "$TMP_DIR/${pkg}_new")
    new_revision=$(awk '/revision:/ { print $2 }' < "$TMP_DIR/${pkg}_new")
    new_pkgver="${pkg}-${new_version}_${new_revision}"

    if [ -z "$new_version" ]; then
        printf >&2 "could not determine version of %s" "$n"
        continue
    fi

    if [ -z "$new_revision" ]; then
        printf >&2 "could not determine revision of %s" "$n"
        continue
    fi

    [ "$cur_pkgver" = "$new_pkgver" ] && continue
    ! version_gte "$new_version" "$cur_version" && continue
    ! version_gte "$new_revision" "$cur_revision" && continue

    printf >&2 "%s%s%s: %s -> %s\\n" \
               "$T_BOLD" \
               "$pkg" \
               "$T_RESET" \
               "$cur_pkgver" \
               "$new_pkgver"
done < "$TMP_DIR/packages"
