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

version_gte() {
    left=$1; shift
    right=$1; shift

    # https://havoc.io/post/shellsemver/
    min_ver="$(printf "%s\\n%s" "$left" "$right" |
        sort -t "." -n -k1,1 -k2,2 -k3,3 -k4,4 |
        head -n 1)"

    if [ "$left" = "$min_ver" ]; then
        printf "1"
    fi
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

# mangle_filename() - takes path, platform and flavors and return a penumeration
# of {path, platform, flavors} pair. If none is given as a platform, then no
# platform resolution will be performed.
mangle_filename() {
    path=$1; shift
    platform=$1; shift
    flavors=$*

    dir=$(dirname "$path")
    base=$(basename "$path")

    printf "%s\\n" "$dir/$base"

    if [ -n "$platform" ] && [ "$platform" != "none" ]; then
        printf "%s\\n" "$dir/$platform/$base"
    fi

    if [ -n "$flavors" ]; then
        for flavor in $flavors; do
            ext=${base##*\.}
            name=${base%*\.${ext}}

            printf "%s\\n" "$dir/$name.$flavor.$ext"

            if [ -n "$platform" ] && [ "$platform" != "none" ]; then
                printf "%s\\n" "$dir/$platform/$name.$flavor.$ext"
            fi
        done
    fi
}

# mangle_file() - similar to mangle_filename() but only returns file that exists.
mangle_file() {
    for f in $(mangle_filename "$@"); do
        if [ -f "$f" ]; then
            printf "%s\\n" "$f"
        fi
    done
}

# mangle_file1() - similar to mangle_file() but only returns the most specific
# file that exists, in an order of: {filename, platform}, {filename}. This
# function doesn't accept flavors, as it doesn't make sense to do so.
mangle_file1() {
    path=$1; shift
    platform=$1; shift

    for f in $(mangle_filename "$path" "$platform"); do
        if [ -f "$f" ]; then
            conf_file="$f"
        fi
    done

    if [ -n "$conf_file" ]; then
        printf "%s\\n" "$conf_file"
    fi
}
