#!/bin/sh

print_usage() {
    echo "Usage: $0 [-h] [-r [app...]] [-e [app...]] [-d [app...]]"
    echo
    echo "    -h   Print this help."
    echo "    -s   Scan for Sparkle-enabled apps."
    echo
    echo "WARNING: Some apps that use a custom derivative of Sparkle"
    echo "cannot be detected by this utility."
}


#
# Utils
#

get_installed_apps() {
    system_profiler SPApplicationsDataType |
        sed -n 's/^ *Location: \(.*\)/\1/p' |
        sort
}

get_sparkle_apps() {
    while read -r _prefix; do
        if [ -d "$_prefix/Contents/Frameworks/Sparkle.framework" ]; then
           echo "$_prefix"
        fi
    done <<< "$(get_installed_apps)"
}

read_plist() {
    _result=$(defaults read "$1" "$2" 2>/dev/null)
    case "$_result" in
        1 | Y* | T* ) echo 1;;
        0 | N* | F* ) echo 0;;
        * ) echo "$_result";;
    esac
}

fmt_state() {
    if [ "$1" = "1" ]; then
        echo "\\033[1;32mENABLED \\033[0;0m"
    elif [ "$1" = "0" ]; then
        echo "\\033[0;31mDISABLED\\033[0;0m"
    elif [ "$1" = "" ]; then
        echo "\\033[0;33mNOT SET \\033[0;0m"
    fi
}


#
# Commands
#

scan_sparkle() {
    while read -r _prefix; do
        _plist="$_prefix/Contents/Info.plist"
        _bundle_id=$(read_plist "$_plist" "CFBundleIdentifier")

        _default_update=$(read_plist "$_plist" "SUEnableAutomaticChecks")
        _current_update=$(read_plist "$_bundle_id" "SUEnableAutomaticChecks")
        _txt_check_update="$(fmt_state "$_default_update") (Default)"
        if [ "$_current_update" != "" ]; then
            _txt_check_update+=" -> $(fmt_state "$_current_update") (Current)"
        fi

        _default_install=$(read_plist "$_plist" "SUAutomaticallyUpdate")
        _current_install=$(read_plist "$_bundle_id" "SUAutomaticallyUpdate")
        _txt_auto_install="$(fmt_state "$_default_install") (Default)"
        if [ "$_current_install" != "" ]; then
            _txt_auto_install+=" -> $(fmt_state "$_current_install") (Current)"
        fi

        echo "\\033[1;37m$_prefix\\033[0;0m"
        echo "\\033[1;34m$_bundle_id\\033[0;0m"
        echo "Check for updates: $_txt_check_update"
        echo "Auto install:      $_txt_auto_install"
        echo
    done <<< "$(get_sparkle_apps)"
}


#
# Args parsing
#

if [ "$*" = "" ]; then
    scan_sparkle
    exit 0
fi

ARGS=$(getopt sh "$@")

# shellcheck disable=SC2181
if [ $? != 0 ];  then
    print_usage
    exit 1
fi

eval set -- "$ARGS"

while true; do
    case "$1" in
        -h ) print_usage; exit 2;;
        -s ) scan_sparkle; exit 0;;
        *  ) break;;
    esac
done
