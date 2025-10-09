#!/usr/bin/env nix-shell
#!nix-shell -i bash -p nix-update cacert ncurses
set -euo pipefail

BASE_DIR=$(
    cd "$(dirname "$0")/.."
    pwd -P
)

cd "$BASE_DIR"
DEBUG=${DEBUG:-0}

c_blue=$(tput setaf 4 2>/dev/null || true)
c_bold=$(tput bold 2>/dev/null || true)
c_yellow=$(tput setaf 3 2>/dev/null || true)
c_red=$(tput setaf 1 2>/dev/null || true)
c_reset=$(tput sgr0 2>/dev/null || true)
c_white=$(tput setaf 7 2>/dev/null || true)

_log_error() {
    printf >&2 "%s[ERROR]%s %s\\n" \
        "$c_red" \
        "$c_reset" \
        "$*"
}

_log_debug() {
    if [ "$DEBUG" = "1" ] || [ "$DEBUG" = "true" ]; then
        printf >&2 "%s[DEBUG]%s %s\\n" \
            "$c_yellow" \
            "$c_reset" \
            "$*"
    fi
}

_log_info() {
    printf >&2 "%s[INFO]%s %s\\n" \
        "$c_blue" \
        "$c_reset" \
        "$*"
}


if [ -n "${NIX_SSL_CERT_FILE:-}" ] && [ -f "${NIX_SSL_CERT_FILE}" ]; then
    export SSL_CERT_FILE="${NIX_SSL_CERT_FILE}"
    export NPM_CONFIG_CAFILE="${NIX_SSL_CERT_FILE}"
    _log_debug "Using CA bundle: ${NIX_SSL_CERT_FILE}"
fi

_cmd() {
    _log_debug "Running command: $*"
    "$@"
}

_update() {
    _cmd nix-shell "$(nix-instantiate --eval --expr '<nixpkgs>')/maintainers/scripts/update.nix" \
        --arg include-overlays '(import ./. { }).overlays' \
        "$@"
}

main() {
    if [ ! -d "$BASE_DIR/.git" ]; then
        _log_error "Needs to be run in a Git project"
        exit 1
    fi

    if ! command -v nix >/dev/null; then
        _log_error "Needs to be run in a Nix environment"
        exit 1
    fi

    if [ -z "$*" ]; then
        _log_info "Running update scripts for known packages"
        _log_info "To run manually, provide args: $0 [args]"
        _log_info "Sleeping for 5 secs before beginning..."
        sleep 5

        _update --argstr skip-prompt true --argstr path local || exit 1
        _update --argstr skip-prompt true --argstr path local.mcpServers || exit 1
        exit 0
    fi

    _update "$@"
}

main "$@"
