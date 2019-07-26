#!/bin/sh -e
#
# Periodically run pfnfsd.sh
#

base_dir=$(cd "$(dirname "$0")" || exit; pwd -P)
interface=$1; shift

if [ "$(id -u)" != "0" ]; then
    printf 2>&1 "Must be run as root\\n"
    exit 1
fi

case $(uname) in
    FreeBSD )
        for svc in rpcbind mountd nfsd; do
            while ! service $svc onestatus >/dev/null 2>&1; do
                sleep 10
            done
        done
        ;;
esac

exec "$base_dir/pfnfsd.sh" -i "$interface" | pfctl -a pfnfsd -f -
