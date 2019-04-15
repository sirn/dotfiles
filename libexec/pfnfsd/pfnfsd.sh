#!/bin/sh -e
#
# This is a script to process output of rpcinfo -p into pf rules.
#

## Args
##

OPTIND=1

print_usage() {
    printf 2>&1 "\
Usage: %s [ARGS...]

Arguments:

    -i EXT_IF     Specify an external interface
    -t ADDR       Specify a remote address or a table name to allow
    -6            Enable IPv6

To use the output of this program with pf, first configure an anchor
in /etc/pf.conf, for example:

    anchor \"rpcinfo\"

Then create an anchor with the following command:

    %s -i ext_if | pfctl -a rpcinfo -f -

All arguments except -i are optional. If -t is not given, it will
default to a <trusted> table.
" "$0" "$0"
}

ext_if=""
from_addr=""
enable_ipv6=0

while getopts "i:t:6h" opt; do
    case "$opt" in
        i ) ext_if="$OPTARG";;
        t ) from_addr="$OPTARG";;
        6 ) enable_ipv6=1;;
        h ) print_usage; exit 2;;
        * ) print_usage; exit 1;;
    esac
done

shift $((OPTIND-1))

if [ -z "$ext_if" ]; then
    print_usage
    exit 1
fi

if [ -z "$from_addr" ]; then
    from_addr="<trusted>"
fi

if [ "${1:-}" = "--" ]; then
    shift
fi


## Main
##

IFS='
'

for line in $(rpcinfo -p); do
    eval set -- "$line"

    proto=$3
    port=$4

    case "$proto" in
        udp | tcp )
            printf \
                "pass in on %s inet proto %s from %s to %s port %s keep state\\n" \
                "$ext_if" "$proto" "$from_addr" "$ext_if" "$port"

            if [ $enable_ipv6 = 1 ]; then
                printf \
                    "pass in on %s inet6 proto %s from %s to %s port %s keep state\\n" \
                    "$ext_if" "$proto" "$from_addr" "$ext_if" "$port"
            fi
            ;;
        * )
            continue
            ;;
    esac
done | sort -u
