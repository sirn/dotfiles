#!/bin/sh -e
#
# Shared functions for Runit
#

install_svc() {
    OPTIND=1

    mkargs=
    svcdir=
    svcvariant=
    svclink=
    program=

    while getopts "Suxp:v:s" opt; do
        case "$opt" in
        S) mkargs="-S" ;;
        u) svcdir="$HOME/.local/var/service" ;;
        v) svcvariant="$OPTARG" ;;
        s) svclink="/run/runit.$USER" ;;
        p) program="$OPTARG" ;;
        *)
            printe_err "Invalid flags given to install_svc"
            exit 1
            ;;
        esac
    done

    shift $((OPTIND - 1))

    if [ "${1:-}" = "--" ]; then
        shift
    fi

    svcsrc=$1
    shift
    svcname=$(basename "$svcsrc")

    if [ -n "$mkargs" ] && [ -n "$svclink" ]; then
        printe_err "install_svc: -S and -s cannot be used together, skipping..."
        return
    fi

    if [ -z "$svcdir" ]; then
        svcdir="/var/service"
    fi

    if [ -n "$svcvariant" ]; then
        svcdir="$svcdir.$svcvariant"
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

    if [ -n "$svclink" ] && [ ! -d "$svclink" ]; then
        USER=${USER:-$(id -un)}
        run_root install -d -o"$USER" -g"$USER" "$svclink"
    fi

    if [ -n "$svclink" ]; then
        install -d "$svclink/supervise.$svcname"
        make_link "$svclink/supervise.$svcname" "$svcsrc/supervise"
    fi

    if [ -f "$svcsrc/log/run" ] && [ -n "$svclink" ]; then
        install -d "$svclink/supervise.$svcname-log"
        make_link "$svclink/supervise.$svcname-log" "$svcsrc/log/supervise"
    fi

    make_link $mkargs "$svcsrc" "$svcdir"
}
