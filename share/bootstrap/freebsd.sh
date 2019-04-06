#!/bin/sh -e

## Service management
##

_service_running() {
    service=$1; shift

    if ! status="$(run_root service "$service" status 2>&1)"; then
        printe_err "$service does not appears to be a valid service, exiting"
        exit 1
    fi

    case "$(printf "%s" "$status" | tr '[:upper:]' '[:lower:]')" in
        *"is running"* | *"enabled"* )
            printf "1"
            ;;
        * )
            printf "0"
            ;;
    esac
}
