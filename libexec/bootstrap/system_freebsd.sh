#!/bin/sh -e
#
# Sets up FreeBSD system.
#

root_dir=${BOOTSTRAP_ROOT:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}
lookup_dir=${LOOKUP_ROOT:-$root_dir}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$root_dir/share/bootstrap/funcs.sh"

if [ "$root_dir" != "$lookup_dir" ]; then
    printe_err "Cannot be included from different root"
    exit 1
fi

if [ "$(uname)" != "FreeBSD" ]; then
    printe_err "Not a FreeBSD system"
    exit 1
fi


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

run_root service nfsd onestart
run_root service mountd onestart
run_root service rpcbind onestart


## PF
##

printe_h2 "Setting up pf..."

pf_updated=0

if file_absent /usr/local/etc/pf.conf; then
    run_root touch /usr/local/etc/pf.conf
    run_root chown root:wheel /usr/local/etc/pf.conf
    run_root chmod 0600 /usr/local/etc/pf.conf
fi

if is_force || [ ! -f /etc/pf.conf ]; then
    run_root cp "$root_dir/etc/pf/pf.freebsd.conf" /etc/pf.conf
    run_root chown root:wheel /etc/pf.conf
    run_root chmod 0600 /etc/pf.conf
    pf_updated=1
else
    printe "/etc/pf.conf already exists, not overwriting"
fi

run_root sysrc pf_enable=YES
run_root service pf onestart

if [ $pf_updated = "1" ]; then
    if ! run_root pfctl -nf /etc/pf.conf; then
        printe_err "/etc/pf.conf has been updated but contained errors, exiting"
        exit 1
    fi

    run_root pfctl -F all -f /etc/pf.conf
    printe "/etc/pf.conf has been updated, internet connection might be interrupted"
fi
