#!/bin/sh -e
#
# Shared functions for Runit
#

install_svc() {
    OPTIND=1

    while getopts "Sup:" opt; do
        case "$opt" in
            S ) mkargs="-S";;
            u ) svcdir="$HOME/.local/var/service";;
            p ) program="$OPTARG";;
            * )
                printe_err "Invalid flags given to make_link"
                exit 1
                ;;
        esac
    done

    shift $((OPTIND-1))

    if [ "${1:-}" = "--" ]; then
        shift
    fi

    svcsrc=$1; shift
    svcname=$(basename "$svcsrc")

    if [ -z "$svcdir" ]; then
        svcdir="/var/service"
    fi

    svcdir="$svcdir/$svcname"
    svcfile="$svcdir/run"

    if [ -n "$program" ] && ! command -v "$program" >/dev/null; then
        printe_info "$program is not installed, skipping..."
        return
    fi

    if ! forced && [ -f "$svcfile" ]; then
        printe_info "$svcname already enabled, skipping..."
        return
    fi

    if [ ! -f "$svcsrc/run" ]; then
        printe_info "$svcsrc does not seems to be runit service, skipping..."
        return
    fi

    make_link $mkargs "$svcsrc" "$svcdir"
}
