#!/bin/sh

echo_clear() {
    printf "\\033[1A\\r\\033[K"
}

echo_ok() {
    printf "\\033[0;32m✓\\033[0;0m %s\\n" "$1"
}

echo_wait() {
    printf "* %s\\n" "$1"
}

echo_error() {
    printf "\\033[0;31m!\\033[0;0m %s\\n" "$1"
}
