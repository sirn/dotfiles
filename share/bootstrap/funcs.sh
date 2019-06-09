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
    printf >&2 "\\033[0;94m==>\\033[0;0m \\033[1;33m%s\\033[0;0m\\n" "$@"
}

printe_h2() {
    printf >&2 "\\033[0;94m==>\\033[0;0m \\033[1;97m%s\\033[0;0m\\n" "$@"
}

printe_info() {
    printf >&2 "\\033[0;94m==>\\033[0;0m %s\\n" "$@"
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

fetch_gh_release() {
    output=$1; shift
    gh_repo=$1; shift
    gh_release=$1; shift
    gh_filename=$1; shift

    fetch_url \
        "$output" \
        "https://github.com/$gh_repo/releases/download/$gh_release/$gh_filename"
}

fetch_gh_raw() {
    output=$1; shift
    gh_repo=$1; shift
    gh_ref=$1; shift
    gh_path=$1; shift

    fetch_url "$output" "https://raw.githubusercontent.com/$gh_repo/$gh_ref/$gh_path"
}

verify_shasum() {
    filepath=$1; shift
    shasum=$1; shift

    if ! command -v sha256 2>/dev/null; then
        printe_info "No sha256 binary found, skipping checksum..."
        return 0
    fi

    echo "$shasum  $filepath" | sha256 -c -
}

file_absent() {
    path=$1; shift

    if [ -e "$path" ]; then
        printe "$path already exists"
        return 1
    fi

    return 0
}

make_temp() {
    build_dir=$(mktemp -d)
    echo "$build_dir"
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

lineinfile() {
    OPTIND=1

    while getopts "f:r:l:s:S" opt; do
        case "$opt" in
            f ) file="$OPTARG";;
            r ) regexp="$OPTARG";;
            l ) line="$OPTARG";;
            s ) state="$OPTARG";;
            S ) command="run_root sh";;
            * )
                printe_err "Invalid flags given to lineinfile"
                exit 1
                ;;
        esac
    done

    if [ -z "$line" ] && [ "$state" != "absent" ]; then
        printe_err "line must be specified unless state is absent"
        exit 1
    fi

    if [ -z "$file" ] || [ ! -f "$file" ]; then
        printe_err "file must be present"
        exit 1
    fi

    if [ -z "$command" ]; then command="sh"; fi
    if [ -z "$state" ];   then state=present; fi
    if [ -z "$regexp" ]; then
        regexp=$(printf "%s" "$line" | sed 's|/|\\\\/|g')
    fi

    case "$state" in
        present )
            $command <<EOF
awk "
    s = /$regexp/ { print \"$line\"; run=1 }
    ! s { print }
    END { if (run != 1) print \"$line\" }
" < "$file" > "$file.new"
mv "$file.new" "$file"
EOF
            ;;
        absent )
            $command <<EOF
awk "! /$regexp/ { print }" < "$file" > "$file.new"
mv "$file.new" "$file"
EOF
            ;;
        * )
            printe_err "Unknown lineinfile state $state"
            exit 1
            ;;
    esac
}


## Guards
##

ensure_platform() {
    platform=$1; shift

    if [ "$(uname)" != "$platform" ]; then
        printe_err "This script can only be run on $platform"
        exit 1
    fi
}

ensure_paths() {
    flags=$*

    if has_args required "$flags"; then
        if [ -z "$BOOTSTRAP_ROOT" ]; then
            printe_err "BOOTSTRAP_ROOT is not set."
            exit 1
        fi

        if [ -z "$LOOKUP_ROOT" ]; then
            printe_err "LOOKUP_ROOT is not set."
            exit 1
        fi
    fi

    if has_args same_root "$flags"; then
        if [ "$BOOTSTRAP_ROOT" != "$LOOKUP_ROOT" ]; then
            printe_err "Cannot be included from different LOOKUP_ROOT"
            exit 1
        fi
    fi
}

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


## Runner
##

run_with_flavors() {
    flavors=$*

    if [ "$(command -v _run)x" != "x" ]; then
        _run "$flavors"
    fi

    for flavor in $flavors; do
        run_command="_run_$flavor"

        if [ "$(command -v "$run_command")x" != "x" ]; then
            "$run_command" "$flavors"
        fi
    done
}


## Cleanups
##

if ! normalize_bool "$NO_CLEAN_BUILDDIR"; then
    trap 'rm -rf $BUILD_DIR' 0 1 2 3 6 14 15
fi
