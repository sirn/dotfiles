#!/bin/sh

run_root() {
    if   hash doas 2>/dev/null; then doas "$@"
    elif hash sudo 2>/dev/null; then sudo "$@"
    else
        printf >&2 "%s: Cannot escalate privileges" "$(basename "$0")"
        printf >&2 "%s: Try installing \`doas\` or \`sudo\`" "$(basename "$0")"
        exit 1
    fi
}

fetch_url() {
    output=$1; shift
    url=$1; shift

    if   hash curl 2>/dev/null;  then curl -sSL -o "$output" "$url"
    elif hash fetch 2>/dev/null; then fetch -q -o "$output" "$url"
    elif hash ftp 2>/dev/null;   then ftp -V -o "$output" "$url"
    else
        printf >&2 "%s: Cannot fetch URL" "$(basename "$0")"
        printf >&2 "%s: Try installing \`curl\`" "$(basename "$0")"
        exit 1
    fi
}

service_running() {
    service=$1; shift

    platform=$(uname | tr '[:upper:]' '[:lower:]')
    platform_svc="_service_running_${platform}"

    if [ "$(command -v "$platform_svc")x" = "x" ]; then
        printf >&2 "%s: Don't know how to check service status for \`$platform\`" "$(basename "$0")"
        exit 1
    fi

    "$platform_svc" "$service"
}

_service_running_darwin() {
    service=$1; shift
    status="$(brew services list | awk "\$1 == \"$service\" { print \$2 }")"

    if [ -z "$status" ]; then
        printf >&2 "$service does not appears to be a valid service, exiting" "$(basename "$0")"
        exit 1
    fi

    if [ "$status" = "started" ]; then
        printf "1"
        return
    fi

    printf "0"
}

_service_running_freebsd() {
    service=$1; shift

    if ! status="$(run_root service "$service" status 2>&1)"; then
        printf >&2 "$service does not appears to be a valid service, exiting" "$(basename "$0")"
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
