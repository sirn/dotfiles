#!/bin/sh -e
#
# Shared functions for Launchd
#

install_launchd() {
    OPTIND=1

    plist_src=
    plist_root=
    plist_dest=
    plist_load=
    plist_dir=

    maybe_root() {
        "$@"
    }

    while getopts "Sd:l" opt; do
        case "$opt" in
        S) plist_root=1 ;;
        d) plist_dir="$OPTARG" ;;
        l) plist_load=1 ;;
        *)
            printe_err "Invalid flags given to install_launchd"
            exit 1
            ;;
        esac
    done

    shift $((OPTIND - 1))

    if [ "${1:-}" = "--" ]; then
        shift
    fi

    if [ "$plist_root" = "1" ]; then
        maybe_root() {
            run_root "$@"
        }
    fi

    if [ -z "$plist_dir" ]; then
        if [ "$plist_root" = "1" ]; then
            plist_dir="/Library/LaunchDaemons"
        else
            plist_dir="$HOME/Library/LaunchAgents"
        fi
    fi

    plist_src=$1
    shift
    plist_filename=$(basename "$plist_src")
    plist_dest="$plist_dir/$plist_filename"

    if [ ! -f "$plist_src" ]; then
        printe_info "$plist_src does not exists, skipping..."
        return
    fi

    if [ -f "$plist_dest" ]; then
        if diff "$plist_src" "$plist_dest" >/dev/null; then
            printe_info "$plist_dest is already present"
            return
        fi

        if [ "$plist_load" = "1" ]; then
            maybe_root launchctl unload "$plist_dest" 2>/dev/null
        fi
    fi

    if [ ! -d "$plist_dir" ]; then
        maybe_root mkdir -p "$plist_dir"
    fi

    maybe_root cp "$plist_src" "$plist_dest"
    maybe_root chmod 0644 "$plist_dest"

    if [ "$plist_root" = "1" ]; then
        maybe_root chown root:staff "$plist_dest"
    fi

    if [ "$plist_load" = "1" ]; then
        maybe_root launchctl load "$plist_dest"
    fi

    printe_info "$plist_dest has been installed"
}

uninstall_launchd() {
    OPTIND=1

    plist_root=
    plist_dest=
    plist_dir=

    maybe_root() {
        "$@"
    }

    while getopts "Sd:" opt; do
        case "$opt" in
        S) plist_root=1 ;;
        d) plist_dir="$OPTARG" ;;
        *)
            printe_err "Invalid flags given to install_svc"
            exit 1
            ;;
        esac
    done

    shift $((OPTIND - 1))

    if [ "${1:-}" = "--" ]; then
        shift
    fi

    if [ "$plist_root" = "1" ]; then
        maybe_root() {
            run_root "$@"
        }
    fi

    if [ -z "$plist_dir" ]; then
        if [ "$plist_root" = "1" ]; then
            plist_dir="/Library/LaunchDaemons"
        else
            plist_dir="$HOME/Library/LaunchAgents"
        fi
    fi

    plist_src=$1
    shift
    plist_filename=$(basename "$plist_src")
    plist_dest="$plist_dir/$plist_filename"

    if [ ! -f "$plist_dest" ]; then
        printe_info "$plist_dest does not exists, skipping..."
        return
    fi

    maybe_root launchctl unload "$plist_dest" 2>/dev/null
    maybe_root rm "$plist_dest"

    printe_info "$plist_dest has been uninstalled"
}
