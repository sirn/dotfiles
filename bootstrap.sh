#!/bin/sh
#
# Script to setup personal workspace.
#

set -e

export LC_ALL=en_US.UTF-8
export PATH=$HOME/.local/bin:/bin:/sbin:/usr/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin

base_dir=$(cd "$(dirname "$0")/" || exit; pwd -P)
platform=$(uname | tr '[:upper:]' '[:lower:]')

cd "$base_dir" || exit 1
. share/bootstrap/compat.sh
. share/bootstrap/funcs.sh


## Arguments handling
##

print_usage() {
    printe "Usage: $0 [OPTS...]"
    printe
    printe "OPTS:"
    printe
    printe "    -p PROFILE      A name of profile to run."
    printe "    -s FLAVOR       A flavor of a profile."
    printe
    printe "PROFILE:"
    printe
    printe "    pkg             Install common packages."
    printe "    system          Configure the system."
    printe "    user            Configure the current user."
    printe
    printe "FLAVOR:"
    printe
    printe "    desktop         Enable desktop flavor."
    printe "    kubernetes      Install Kubernetes tools."
    printe
    printe "Bootstrap script will default to \`pkg\` without any flavors"
    printe "if no profiles and flavors is given."
    printe
}

OPTIND=1

profiles=""
flavors=""

while getopts "p:s:" opt; do
    case "$opt" in
        p ) profiles="$profiles $OPTARG";;
        s ) flavors="$flavors $OPTARG";;
        * ) print_usage; exit 1;;
    esac
done

shift $((OPTIND-1))

if [ "${1:-}" = "--" ]; then
    shift
fi


## Sanity check
##

for p in $profiles; do
    case "$p" in
        pkg | system | user ) ;;
        * ) printe_err "Unknown profile \`$p\`"; exit 1;;
    esac
done

for f in $flavors; do
    case "$f" in
        desktop | kubernetes ) ;;
        * ) printe_err "Unknown flavor \`$f\`"; exit 1;;
    esac
done

if [ -z "$profiles" ]; then
    profiles="pkg"
fi


## Running
##

for p in pkg system user; do
    if [ "$(has_args "$p" "$profiles")" = "1" ]; then
        runscript="$base_dir/libexec/bootstrap/${p}_${platform}.sh"

        if [ ! -f "$runscript" ]; then
            printe_err "Profile \`${p}\` was not found for platform \`${platform}\`"
            exit 1
        fi

        printe_h1 "Running ${runscript##$base_dir/}..."
        "$runscript" "$flavors"
    fi
done
