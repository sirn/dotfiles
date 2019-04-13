#!/bin/sh -e
#
# Sets up OpenBSD system.
#

root_dir=${BOOTSTRAP_ROOT:-$(cd "$(dirname "$0")/../.." || exit; pwd -P)}
lookup_dir=${LOOKUP_ROOT:-$root_dir}

# shellcheck source=../../share/bootstrap/funcs.sh
. "$root_dir/share/bootstrap/funcs.sh"

if [ "$root_dir" != "$lookup_dir" ]; then
    printe_err "Cannot be included from different root"
    exit 1
fi

if [ "$(uname)" != "OpenBSD" ]; then
    printe_err "Not an OpenBSD system"
    exit 1
fi


## NFS
##

printe_h2 "Setting up nfsd..."

if [ ! -f /etc/exports ]; then
    run_root touch /etc/exports
fi

run_root rcctl enable portmap mountd nfsd
run_root rcctl start portmap mountd nfsd


## PF
##

printe_h2 "Setting up pf..."

if file_absent /etc/pf.conf.local; then
    run_root touch /etc/pf.conf.local
    run_root chown root:wheel /etc/pf.conf.local
    run_root chmod 0600 /etc/pf.conf.local
fi

if is_force || [ ! -f /etc/pf.conf ]; then
    run_root cp "$root_dir/etc/pf/pf.openbsd.conf" /etc/pf.conf
    run_root chown root:wheel /etc/pf.conf
    run_root chmod 0600 /etc/pf.conf

    if ! run_root pfctl -nf /etc/pf.conf; then
        printe_err "/etc/pf.conf has been updated but contained errors, exiting"
        exit 1
    fi

    run_root pfctl -F all -f /etc/pf.conf
    printe "/etc/pf.conf has been updated, internet connection might be interrupted"
else
    printe "/etc/pf.conf already exists, not overwriting"
fi
