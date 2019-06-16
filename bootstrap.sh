#!/bin/sh -e
#
# Script to setup personal workspace.
#

BASE_DIR=$(cd "$(dirname "$0")/" || exit; pwd -P)
PLATFORM=$(uname | tr '[:upper:]' '[:lower:]')

LC_ALL=en_US.UTF-8
PATH=/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin
PATH=$HOME/.local/bin:/opt/local/bin:$PATH

export LC_ALL
export PATH

cd "$BASE_DIR" || exit 1
. share/bootstrap/funcs.sh


## Arguments handling
##

USAGE_MSG="\
Usage: $0 [OPTS...]

OPTS:

    -p PROFILE      A name of profile to run.
    -s FLAVOR       A flavor of a profile.
    -l PATH         Configure a lookup path.
    -f              Force executing scripts.

PROFILE:

    pkg             Install common packages.
    system          Configure the system.
    user            Configure the current user.

FLAVOR:

    backups         Install backup packages.
    desktop         Install desktop packages.
    dev             Install development packages.
    kubernetes      Install Kubernetes packages.
    mail            Install mail packages.

Bootstrap script will default to \`pkg\` without any FLAVORS
if no PROFILES and FLAVORS is given. Lookup path is default to
the current directory and ~/.dotpriv.
" # EOF

print_usage() {
    printe "$USAGE_MSG"
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
        pkg | system | user ) ;;
        * ) printe_err "Unknown profile: $p"; exit 1;;
    esac
done

for f in $FLAVORS; do
    case "$f" in
        dev | desktop | kubernetes | mail | backups ) ;;
        * ) printe_err "Unknown flavor: $f"; exit 1;;
    esac
done

if [ -z "$PROFILES" ]; then
    PROFILES="pkg"
fi


## Running
##

for p in pkg system user; do
    if has_args "$p" "$PROFILES"; then
        run=0

        for b in $LOOKUP_PATH; do
            runscript="$b/libexec/bootstrap/${p}_${PLATFORM}.sh"

            if [ ! -f "$runscript" ]; then
                continue
            fi

            LOOKUP_ROOT="$b"; export LOOKUP_ROOT
            BOOTSTRAP_ROOT="$BASE_DIR"; export BOOTSTRAP_ROOT

            run=1
            printe_h1 "Running ${runscript}..."
            "$runscript" "$FLAVORS"
        done

        if [ $run != 1 ]; then
            printe_err "Profile ${p} was not found for PLATFORM ${PLATFORM}"
            exit 1
        fi
    fi
done
