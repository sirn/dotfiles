#!/bin/sh -e
#
# Sets up FreeBSD system.
#

base_dir=$(cd "$(dirname "$0")/" || exit; pwd -P)
flavors=$*

cd "$base_dir" || exit 1
. ../../share/bootstrap/funcs.sh
. ../../share/bootstrap/compat.sh

if [ "$(uname)" != "FreeBSD" ]; then
    printe_err "Not a FreeBSD system"
    exit 1
fi


## Utils
##

_start_if_not() {
    service=$1; shift

    if ! status="$(run_root service "$service" status 2>&1)"; then
        printe_err "$service does not appears to be a valid service, exiting"
        exit 1
    fi

    case "$(printf "%s" "$status" | tr '[:upper:]' '[:lower:]')" in
        *"is running"* | *"enabled"* ) ;;
        * ) run_root service "$service" onestart;;
    esac
}


## NFS
##

printe_h2 "Setting up nfsd..."

if [ ! -f /etc/exports ]; then
    run_root touch /etc/exports
fi

run_root sysrc nfs_server_enable=YES
run_root sysrc mountd_enable=YES
run_root sysrc rpcbind_enable=YES
run_root sysrc rpc_lockd_enable=YES
run_root sysrc rpc_statd_enable=YES

_start_if_not nfsd
_start_if_not mountd
_start_if_not rpcbind


## PF
##

printe_h2 "Setting up pf..."

pf_updated=0

if [ "$(normalize_bool "$FORCE")" = "1" ] || [ ! -f /etc/pf.conf ]; then
    run_root cp ../../share/examples/bootstrap/pf.conf /etc/pf.conf
    run_root chown root:wheel /etc/pf.conf
    run_root chmod 0600 /etc/pf.conf
    pf_updated=1
else
    printe_msg "/etc/pf.conf is already exists, not overwriting"
fi

run_root sysrc pf_enable=YES

_start_if_not pf

if [ "$pf_updated" = "1" ]; then
    if ! run_root pfctl -nf /etc/pf.conf; then
        printe_err "/etc/pf.conf was updated but contained errors, not proceeding"
        exit 1
    fi

    run_root pfctl -F all -f /etc/pf.conf
    printe "pf reloaded, internet connection might be interrupted"
fi
