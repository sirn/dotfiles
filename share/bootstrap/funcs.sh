#!/bin/sh

## Printing
##

printe() {
    printf >&2 "%s\\n" "$@"
}

printe_h1() {
    printf >&2 "\033[0;94m==>\\033[0;0m \\033[1;33m%s\\033[0;0m\\n" "$@"
}

printe_h2() {
    printf >&2 "\033[0;94m==>\\033[0;0m \\033[1;97m%s\\033[0;0m\\n" "$@"
}

printe_info() {
    printf >&2 "\033[0;94m==>\\033[0;0m %s\\n" "$@"
}

printe_err() {
    printe "$(basename "$0"): $*"
}


## Normalizing
##

trim() {
    var="$1"
    var="${var#"${var%%[![:space:]]*}"}"
    var="${var%"${var##*[![:space:]]}"}"
    printf "%s" "$var"
}

normalize_bool() {
    value=$1; shift

    case "$value" in
        t* | T* | y* | Y* | 1 ) printf "1";;
        * ) printf "0";;
    esac
}


## Comparison
##

has_args() {
    needle=$1; shift
    haystack=$1; shift

    case "$haystack" in
        *" $needle" | "$needle "* | "$needle" | *" $needle "* )
            printf "1"
            ;;
        * )
            printf "0"
            ;;
    esac
}


## Convenient funcs
##

git_clone_update() {
    repo=$1; shift
    path=$1; shift

    if [ ! -d "$path" ]; then
        git clone "$repo" "$path"
    else
        git -C "$path" checkout -q master
        git -C "$path" pull -q origin master
        printe "$path has been successfully updated"
    fi
}
