#!/bin/sh

## Const
##

# shellcheck disable=SC2034
NEWLINE='
'

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


## Compat
##

run_root() {
    if   hash doas 2>/dev/null; then doas "$@"
    elif hash sudo 2>/dev/null; then sudo "$@"
    else
        print_err "Cannot escalate privileges"
        printe_err "%s: Try installing \`doas\` or \`sudo\`"
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
        printe_err "Cannot fetch URL"
        printe_err "Try installing \`curl\`"
        exit 1
    fi
}


## Normalizing
##

trim() {
    var=$1
    var=${var#"${var%%[![:space:]]*}"}
    var=${var%"${var##*[![:space:]]}"}
    printf "%s" "$var"
}

normalize_bool() {
    value=$1; shift

    case $value in
        t* | T* | y* | Y* | 1 ) return 0;;
        * ) return 1;;
    esac
}


## Comparison
##

has_args() {
    needle=$1; shift
    haystack=$1; shift

    case $haystack in
        *" $needle" | "$needle "* | "$needle" | *" $needle "* )
            return 0
            ;;
        * )
            return 1
            ;;
    esac
}

version_gte() {
    left=$1; shift
    right=$1; shift

    # https://havoc.io/post/shellsemver/
    min_ver=$(printf "%s\\n%s" "$left" "$right" |
        sort -t "." -n -k1,1 -k2,2 -k3,3 -k4,4 |
        head -n 1)

    if [ "$right" = "$min_ver" ]; then
        return 0
    fi

    return 1
}


## Convenient funcs
##

is_force() {
    normalize_bool "$FORCE"
}

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

fetch_gh_archive() {
    output=$1; shift
    gh_repo=$1; shift
    gh_ref=$1; shift

    fetch_url "$output" "https://github.com/$gh_repo/archive/$gh_ref.tar.gz"
}

fetch_gh_raw() {
    output=$1; shift
    gh_repo=$1; shift
    gh_ref=$1; shift
    gh_path=$1; shift

    fetch_url "$output" "https://raw.githubusercontent.com/$gh_repo/$gh_ref/$gh_path"
}

file_absent() {
    path=$1; shift

    if [ -e "$path" ]; then
        printe "$path already exists"
        return 1
    fi

    return 0
}

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

make_link() {
    src=$1; shift
    dest=$1; shift

    if [ ! -e "$src" ]; then
        printe "$src doesn't exists, skipping..."
        return
    fi

    if [ -f "$dest" ] && [ ! -L "$dest" ]; then
        printe "$dest already exists and is not a link, skipping"
        return
    fi

    if [ "$(readlink "$dest")" = "$src" ]; then
        printe "$dest already linked"
        return
    fi

    mkdir -p "$(dirname "$dest")"
    rm -f "$dest"
    ln -s "$src" "$dest"
    printe "$dest has been linked to $src"
}


## Reqs
##

require_bin() {
    pkg=$1; shift
    desc=$1

    if ! command -v "$pkg" >/dev/null; then
        printe_err "$pkg could not be found on the system"

        if [ -n "$desc" ]; then
            printe_err "$desc"
        fi

        exit 1
    fi
}

require_lib() {
    lib=$1; shift
    desc=$1

    if ! pkg-config "$lib"; then
        printe_err "$lib is required to be installed"

        if [ -n "$desc" ]; then
            printe_err "$desc"
        fi

        exit 1
    fi
}
