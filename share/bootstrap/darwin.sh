#!/bin/sh -e

## Service management
##

_service_running() {
    service=$1; shift
    status="$(brew services list | awk "\$1 == \"$service\" { print \$2 }")"

    if [ -z "$status" ]; then
        printe_err "$service does not appears to be a valid service, exiting"
        exit 1
    fi

    if [ "$status" = "started" ]; then
        printf "1"
        return
    fi

    printf "0"
}
