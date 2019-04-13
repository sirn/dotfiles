#!/bin/sh
#
# Script to setup personal workspace.
#

set -e

base_dir=$(cd "$(dirname "$0")/" || exit; pwd -P)
platform=$(uname | tr '[:upper:]' '[:lower:]')

LC_ALL=en_US.UTF-8
PATH=$HOME/.local/bin:/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin

export LC_ALL
export PATH

cd "$base_dir" || exit 1
. share/bootstrap/funcs.sh


## Arguments handling
##

usage_msg="\
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

    desktop         Install desktop packages.
    kubernetes      Install Kubernetes packages.
    mail            Install mail packages.

Bootstrap script will default to \`pkg\` without any flavors
if no profiles and flavors is given. Lookup path is default to
the current directory and ~/.dotpriv.
" # EOF

print_usage() {
    printe "$usage_msg"
}

OPTIND=1
FORCE=0

profiles=""
flavors=""
lookup_path=""

while getopts "hp:s:l:f" opt; do
    case "$opt" in
        p ) profiles="$profiles $OPTARG";;
        s ) flavors="$flavors $OPTARG";;
        l ) lookup_path="$lookup_path $OPTARG";;
        f ) FORCE=1;;
        h ) print_usage; exit 2;;
        * ) print_usage; exit 1;;
    esac
done

shift $((OPTIND-1))

if [ -z "$lookup_path" ]; then
    lookup_path="$base_dir $HOME/.dotpriv"
fi

if [ "${1:-}" = "--" ]; then
    shift
fi

export FORCE


## Sanity check
##

for p in $profiles; do
    case "$p" in
        pkg | system | user ) ;;
        * ) printe_err "Unknown profile: $p"; exit 1;;
    esac
done

for f in $flavors; do
    case "$f" in
        desktop | kubernetes | mail ) ;;
        * ) printe_err "Unknown flavor: $f"; exit 1;;
    esac
done

if [ -z "$profiles" ]; then
    profiles="pkg"
fi


## Running
##

for p in pkg system user; do
    if has_args "$p" "$profiles"; then
        run=0

        for b in $lookup_path; do
            runscript="$b/libexec/bootstrap/${p}_${platform}.sh"

            if [ ! -f "$runscript" ]; then
                continue
            fi

            LOOKUP_ROOT="$b"; export LOOKUP_ROOT
            BOOTSTRAP_ROOT="$base_dir"; export BOOTSTRAP_ROOT

            run=1
            printe_h1 "Running ${runscript}..."
            "$runscript" "$flavors"
        done

        if [ $run != 1 ]; then
            printe_err "Profile ${p} was not found for platform ${platform}"
            exit 1
        fi
    fi
done
