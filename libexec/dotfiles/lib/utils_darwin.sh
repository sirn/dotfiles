#!/bin/sh -e
#
# Shared functions for Darwin
#

macports_installed() {
    pkg=$1
    shift
    variants=$*
    active=$(port -q installed "$pkg" 2>&1 | grep "(active)")

    # Not installed
    if [ -z "$active" ]; then
        return 1
    fi

    # Installed, but not the requested variant
    for v in $variants; do
        case "$v" in
        +*)
            if echo "$active" | grep -qv -- "$v"; then
                return 1
            fi
            ;;
        -*)
            if echo "$active" | grep -q -- "$v"; then
                return 1
            fi
            ;;
        esac
    done

    return 0
}

macports_install() {
    pkg=$1
    shift

    if macports_installed "$pkg" "$@"; then
        printe_info "$pkg (macports) already installed"
        return
    fi

    printe_info "$pkg (macports), not installed, installing..."

    if ! run_root port -N install "$pkg" "$@"; then
        printe_info "$pkg (macports) failed to install, skipping..."
    fi
}

macports_select() {
    sel=$1
    shift
    pkg=$1
    shift

    case $(port select --show "$sel") in
    *"'$sel' is '$pkg'"*)
        printe_info "$pkg is already default version for $sel, skipping..."
        return
        ;;
    esac

    printe_info "Selecting $pkg as default version for $sel (macports)..."
    run_root port select "$sel" "$pkg"
}
