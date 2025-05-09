#!/bin/sh

BASE_DIR=$(
    cd "$(dirname "$0")/.." || exit
    pwd -P
)

## Configurables
## ----------------------------------------------------------------------------

SOURCE_URL="https://files.grid.in.th/ssh.keys"

SERVER_LIST=$(gpg --decrypt "$BASE_DIR/var/server-list.txt.gpg" 2>/dev/null)

## Internal
## ----------------------------------------------------------------------------
REROLL_LOGLEVEL=${REROLL_LOGLEVEL:-"info"}
REROLL_TEMPDIR=$(mktemp -d)

PROG=$(basename "$0")

trap 'find $REROLL_TEMPDIR/ -type f -delete; rmdir $REROLL_TEMPDIR' 0 1 2

## Messages
## -----------------------------------------------------------------------------

c_blue=$(tput setaf 4 2>/dev/null || true)
c_bold=$(tput bold 2>/dev/null || true)
c_yellow=$(tput setaf 3 2>/dev/null || true)
c_red=$(tput setaf 1 2>/dev/null || true)
c_reset=$(tput sgr0 2>/dev/null || true)
c_white=$(tput setaf 7 2>/dev/null || true)

msg() {
    printf >&2 "%s[%s]%s %s\\n" \
        "$c_blue" "$PROG" "$c_reset" \
        "$*"
}

case "$REROLL_LOGLEVEL" in
debug)
    REROLL_LOGLEVEL_I=0
    ;;
warn | warning)
    REROLL_LOGLEVEL_I=2
    ;;
error)
    REROLL_LOGLEVEL_I=3
    ;;
*) # Info
    REROLL_LOGLEVEL_I=1
    ;;
esac

if [ "$REROLL_LOGLEVEL_I" -lt 1 ]; then
    msg_debug() {
        printf >&2 "%s[%s]%s %sDEBUG:%s %s\\n" \
            "$c_blue" "$PROG" "$c_reset" \
            "$c_bold" "$c_reset" "$*"
    }
else
    msg_debug() {
        :
    }
fi

if [ "$REROLL_LOGLEVEL_I" -lt 2 ]; then
    msg_info() {
        printf >&2 "%s[%s]%s %sINFO: %s%s\\n" \
            "$c_blue" "$PROG" "$c_reset" \
            "$c_white" "$*" "$c_reset"
    }

    msg_info_verbose() {
        if [ "$REROLL_VERBOSE" = "1" ]; then
            printf >&2 "%s[%s]%s %sVERBOSE: %s%s\\n" \
                "$c_blue" "$PROG" "$c_reset" \
                "$c_white" "$*" "$c_reset"
        fi
    }
else
    msg_info() {
        :
    }

    msg_info_verbose() {
        :
    }
fi

if [ "$REROLL_LOGLEVEL_I" -lt 3 ]; then
    msg_warn() {
        printf >&2 "%s[%s]%s %sWARNING: %s%s\\n" \
            "$c_blue" "$PROG" "$c_reset" \
            "$c_yellow" "$*" "$c_reset"
    }
else
    msg_warn() {
        :
    }
fi

if [ "$REROLL_LOGLEVEL_I" -lt 4 ]; then
    msg_error() {
        printf >&2 "%s[%s]%s %sERROR: %s%s\\n" \
            "$c_blue" "$PROG" "$c_reset" \
            "$c_red" "$*" "$c_reset"
    }
else
    msg_error() {
        :
    }
fi

cmd_run_nd() {
    msg_debug "Command [no dry-run]: $*"
    RET=0
    "$@" 2>/dev/null
    RET=$?
    msg_debug "Command exit: $RET"
    return $RET
}

cmd_run() {
    msg_debug "Command: $*"

    RET=0
    if [ "$REROLL_DRYRUN" = "1" ]; then
        echo >&2 "$@"
    else
        "$@"
        RET=$?
    fi

    msg_debug "Command exit: $RET"
    return $RET
}

## roll-ssh-keys help
## ----------------------------------------------------------------------------

reroll_help() {
    cat >&2 <<EOF
Usage: $PROG [OPTIONS] COMMAND [SUBCOMMAND...]

Run \`$PROG COMMAND help' for further subcommands.

COMMAND:

    check
        Print current .ssh/authorized_keys in all servers

    update
        Update all .ssh/authorized_keys to SOURCE_URL

SETTINGS:

    SOURCE_URL=$SOURCE_URL

OPTIONS

    -v
        Verbose logging

EOF
}

## roll-ssh-keys check
## ----------------------------------------------------------------------------

reroll_check() {
    echo "$SERVER_LIST" | grep -E '^[^#]+' | while read -r n; do
        if [ -z "$n" ]; then
            continue
        fi

        _hostname="${n%%:*}"
        _port="${n##"${_hostname}":}"
        if [ -z "$_port" ]; then
            _port=22
        fi

        msg_info "Checking ${_hostname}:${_port}"
        cmd_run ssh -n -p "${_port}" "$_hostname" cat .ssh/authorized_keys
    done
}

## roll-ssh-keys update
## ----------------------------------------------------------------------------

reroll_update() {
    _authorized_keys="$REROLL_TEMPDIR"/authorized_keys
    curl -sSL "$SOURCE_URL" -o "$REROLL_TEMPDIR"/authorized_keys

    echo "$SERVER_LIST" | grep -E '^[^#]+' | while read -r n; do
        if [ -z "$n" ]; then
            continue
        fi

        _hostname="${n%%:*}"
        _port="${n##"${_hostname}":}"
        if [ -z "$_port" ]; then
            _port=22
        fi

        msg_info "Updating ${_hostname}:${_port}"
        cmd_run cat "$_authorized_keys" |
            cmd_run ssh -p "${_port}" "$_hostname" dd of=.ssh/authorized_keys
    done
}

## Entrypoint
## ----------------------------------------------------------------------------

main() {
    msg_debug "env PROG=${PROG}"
    msg_debug "env REROLL_TEMPDIR=${REROLL_TEMPDIR}"

    REROLL_VERBOSE=0
    OPTIND=1

    while getopts "v" opt; do
        case "$opt" in
        v) REROLL_VERBOSE=1 ;;
        *) ;;
        esac
    done

    msg_debug "env REROLL_VERBOSE=$REROLL_VERBOSE"
    shift $((OPTIND - 1))

    if [ "${1:-}" = "--" ]; then
        shift
    fi

    REROLL_COMMAND=
    if [ "$#" -gt 0 ]; then
        REROLL_COMMAND=$1
        shift
        msg_debug "env REROLL_COMMAND=$REROLL_COMMAND"
    fi

    case "$REROLL_COMMAND" in
    check)
        reroll_check "$@"
        ;;
    update)
        reroll_update "$@"
        ;;
    *)
        reroll_help "$@"
        exit 2
        ;;
    esac
}

main "$@"
