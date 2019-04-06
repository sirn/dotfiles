#!/bin/sh

run_root() {
    if   hash doas 2>/dev/null; then doas "$@"
    elif hash sudo 2>/dev/null; then sudo "$@"
    else
        printf >&2 "%s: Cannot escalate privileges" "$0"
        printf >&2 "%s: Try installing \`doas\` or \`sudo\`" "$0"
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
        printf >&2 "%s: Cannot fetch URL" "$0"
        printf >&2 "%s: Try installing \`curl\`" "$0"
        exit 1
    fi
}
