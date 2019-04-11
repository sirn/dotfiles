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

    if [ "$left" = "$min_ver" ]; then
        return 0
    fi

    return 1
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


## Services management
##

service_running() {
    service=$1; shift

    platform=$(uname | tr '[:upper:]' '[:lower:]')
    platform_svc=_service_running_$platform

    if [ "$(command -v "$platform_svc")x" = "x" ]; then
        printe_err "Don't know how to check service status for \`$platform\`"
        exit 1
    fi

    "$platform_svc" "$service"
}

_service_running_darwin() {
    service=$1; shift
    status=$(brew services list | awk "\$1 == \"$service\" { print \$2 }")

    if [ -z "$status" ]; then
        printe_err "$service does not appears to be a valid service, exiting"
        exit 1
    fi

    if [ "$status" = "started" ]; then
        printf "1"
        return
    fi

    printf "0"
}

_service_running_freebsd() {
    service=$1; shift

    if ! status=$(run_root service "$service" status 2>&1); then
        printe_err "$service does not appears to be a valid service, exiting"
        exit 1
    fi

    case $(printf "%s" "$status" | tr '[:upper:]' '[:lower:]') in
        *"is running"* | *"enabled"* )
            printf "1"
            ;;
        * )
            printf "0"
            ;;
    esac
}


## Reqs
##

require_autoconf() {
    what=$1; shift

    if ! command -v autoconf >/dev/null; then
        printe_err "Building $what on $(uname) requires autoconf"

        case $(uname) in
            OpenBSD ) printe_err "Try \`pkg_add metaauto autoconf%2.69\`";;
            FreeBSD ) printe_err "Try \`pkg install autoconf\`";;
            Darwin )  printe_err "Try \`brew install autoconf\`";;
        esac

        exit 1
    fi
}

require_bash() {
    what=$1; shift

    if ! command -v bash >/dev/null; then
        printe_err "Building $what on $(uname) requires bash"

        case $(uname) in
            OpenBSD ) printe_err "Try \`pkg_add bash\`";;
            FreeBSD ) printe_err "Try \`pkg install bash\`";;
            Darwin )  printe_err "Try \`brew install bash\`";;
        esac

        exit 1
    fi
}

require_coreutils() {
    what=$1; shift

    if ! command -v gls >/dev/null; then
        printe_err "Building $what on $(uname) requires coreutils"

        case $(uname) in
            OpenBSD ) printe_err "Try \`pkg_add coreutils\`";;
            FreeBSD ) printe_err "Try \`pkg install coreutils\`";;
            Darwin )  printe_err "Try \`brew install coreutils\`";;
        esac

        exit 1
    fi
}

require_gmake() {
    what=$1; shift

    if ! command -v gmake >/dev/null; then
        printe_err "Building $what on $(uname) requires gmake"

        case $(uname) in
            OpenBSD ) printe_err "Try \`pkg_add gmake\`";;
            FreeBSD ) printe_err "Try \`pkg install gmake\`";;
            Darwin )  printe_err "Try \`brew install gmake\`";;
        esac

        exit 1
    fi
}

require_gtar() {
    what=$1; shift

    if ! command -v gtar >/dev/null; then
        printe_err "Building $what on $(uname) requires gtar"

        case $(uname) in
            OpenBSD ) printe_err "Try \`pkg_add gtar\`";;
            FreeBSD ) printe_err "Try \`pkg install gtar\`";;
            Darwin )  printe_err "Try \`brew install gtar\`";;
        esac

        exit 1
    fi
}

require_go() {
    what=$1; shift

    if ! command -v go >/dev/null; then
        printe_err "Building $what on $(uname) requires go"

        case $(uname) in
            OpenBSD ) printe_err "Try \`pkg_add go\`";;
            FreeBSD ) printe_err "Try \`pkg install go\`";;
            Darwin )  printe_err "Try \`brew install go\`";;
        esac

        exit 1
    fi
}

require_brew_sqlite3() {
    what=$1; shift

    case $(uname) in
        Darwin )
            if [ ! -d /usr/local/opt/sqlite3 ]; then
                printe_err "Building $what on Darwin requires sqlite3"
                printe_err "Try \`brew install sqlite3\`"
                exit 1
            fi
            ;;
    esac
}

require_brew_zlib() {
    what=$1; shift

    case $(uname) in
        Darwin )
            if [ ! -d /usr/local/opt/zlib ]; then
                printe_err "Building $what on Darwin requires zlib"
                printe_err "Try \`brew install zlib\`"
                exit 1
            fi
            ;;
    esac
}
