#!/bin/sh -e
#
# Script to setup personal workspace.
#

BASE_DIR=$(cd "$(dirname "$0")/" || exit; pwd -P)
cd "$BASE_DIR" || exit 1

. var/dotfiles/lib/utils.sh


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
    -l PATH         Configure a lookup path.
    -f              Force executing scripts.

PROFILE:

    pkg             Install common packages.
    user            Configure the current user.

FLAVOR:

    backups         Install backup packages.
    desktop         Install desktop packages.
    dev             Install development packages.
    mail            Install mail packages.
    system          Install system packages.

Bootstrap script will default to \`pkg\` without any FLAVORS
if no PROFILES and FLAVORS is given. Lookup path is default to
the current directory and ~/.dotpriv.
" # EOF
}

OPTIND=1
FORCE=0

PROFILES=""
FLAVORS=""
LOOKUP_PATH=""

while getopts "hp:s:l:f" opt; do
    case "$opt" in
        p ) PROFILES="$PROFILES $OPTARG";;
        s ) FLAVORS="$FLAVORS $OPTARG";;
        l ) LOOKUP_PATH="$LOOKUP_PATH $OPTARG";;
        f ) FORCE=1;;
        h ) print_usage; exit 2;;
        * ) print_usage; exit 1;;
    esac
done

shift $((OPTIND-1))

if [ -z "$LOOKUP_PATH" ]; then
    LOOKUP_PATH="$BASE_DIR $HOME/.dotpriv"
fi

if [ "${1:-}" = "--" ]; then
    shift
fi

export FORCE


## Sanity check
##

for p in $PROFILES; do
    case "$p" in
        pkg | user ) ;;
        * ) printe_err "Unknown profile: $p"; exit 1;;
    esac
done

for f in $FLAVORS; do
    case "$f" in
        dev | desktop | mail | backups | system ) ;;
        * ) printe_err "Unknown flavor: $f"; exit 1;;
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
    if has_args "$p" "$PROFILES"; then
        run=0

        for b in $LOOKUP_PATH; do
            runscript="$b/var/dotfiles/${p}_${SYS}.sh"

            if [ ! -f "$runscript" ]; then
                continue
            fi

            run=1
            printe_h1 "Running ${runscript}..."
            (
                cd "$(dirname "$runscript")" || exit 1
                # shellcheck disable=SC1090
                . "$runscript"
                run_with_flavors "$FLAVORS"
            )
        done

        if [ $run != 1 ]; then
            printe_err "Profile ${p} was not found for ${SYS}"
            exit 1
        fi
    fi
done
