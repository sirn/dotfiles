#!/bin/sh
#
# Find packages that are locally built.
#

if ! command -v xbps-query >/dev/null; then
    printf >&2 "xbps-query not found, not a Void Linux system?\\n"
    exit 1
fi

TMP_DIR=$(mktemp -d)
trap 'rm -rf $TMP_DIR' 0 1 2 3 6 14 15

TMP_PKGLIST=$TMP_DIR/xbps_packages
xbps-query -s '^/' --property repository --regex > "$TMP_PKGLIST"

while read -r n; do
    pkg=${n%: *}
    printf "%s\\n" "${pkg%-*}"
done < "$TMP_PKGLIST"
