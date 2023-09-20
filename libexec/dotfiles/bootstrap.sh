#!/bin/sh -e
#
# Script to setup personal workspace.
#
#shellcheck disable=SC1091

BASE_DIR=$(
    cd "$(dirname "$0")/../.." || exit
    pwd -P
)

cd "$BASE_DIR" || exit 1

. "$BASE_DIR/libexec/dotfiles/lib/utils.sh"

## Environment variables
##

LC_ALL=en_US.UTF-8
PATH=/usr/bin:/usr/sbin:/bin:/sbin:/usr/local/bin:/usr/local/sbin
PATH=$HOME/.local/bin:/opt/local/bin:$PATH
SYS=$(get_sys)

export LC_ALL
export PATH

## Arguments handling
##

print_usage() {
    printe "\
Usage: $0 [OPTS...]

OPTS:

    -p PROFILE      A name of profile to run.
    -s FLAVOR       A flavor of a profile.
    -f              Force executing scripts.

PROFILE:

    pkg             Install common packages.
    user            Configure the current user.

FLAVOR:

    desktop         Install desktop packages.
    system          Install system packages.
    sway            Install sway packages.

Bootstrap script will default to \`pkg\` without any FLAVORS
if no PROFILES and FLAVORS is given.
" # EOF
}

OPTIND=1
FORCE=0

PROFILES=""
FLAVORS=""

while getopts "hp:s:l:f" opt; do
    case "$opt" in
    p) PROFILES="$PROFILES $OPTARG" ;;
    s) FLAVORS="$FLAVORS $OPTARG" ;;
    f) FORCE=1 ;;
    h)
        print_usage
        exit 2
        ;;
    *)
        print_usage
        exit 1
        ;;
    esac
done

shift $((OPTIND - 1))

if [ "${1:-}" = "--" ]; then
    shift
fi

export FORCE

## Sanity check
##

for p in $PROFILES; do
    case "$p" in
    pkg | user) ;;
    *)
        printe_err "Unknown profile: $p"
        exit 1
        ;;
    esac
done

for f in $FLAVORS; do
    case "$f" in
    desktop | system | sway) ;;
    *)
        printe_err "Unknown flavor: $f"
        exit 1
        ;;
    esac
done

if [ -z "$PROFILES" ]; then
    PROFILES="pkg"
fi

if [ "$(id -u)" = "0" ]; then
    printe_err "Cannot run as root"
    exit 2
fi

## Running
##

for p in pkg user; do
    runscript="$BASE_DIR/libexec/dotfiles/${p}_${SYS}.sh"

    if [ ! -f "$runscript" ]; then
        printe_err "Profile ${p} was not found for ${SYS}"
        exit 1
    fi

    printe_h1 "Running ${runscript}..."
    (
        cd "$(dirname "$runscript")" || exit 1
        # shellcheck disable=SC1090
        . "$runscript"
        run_with_flavors "$FLAVORS"
    )
done
