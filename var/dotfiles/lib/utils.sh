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
    printf >&2 "%s: %s\\n" "$(basename "$0")" "$@"
}


## Compat
##

run_root() {
    if command -v doas >/dev/null; then
        doas "$@"
    elif command -v sudo >/dev/null; then
        sudo "$@"
    else
        printe_err "Cannot escalate privileges"
        printe_err "Try installing \`doas\` or \`sudo\`"
        exit 1
    fi
}

run_tar() {
    if command -v bsdtar >/dev/null; then
        bsdtar "$@"
    elif command -v gtar >/dev/null; then
        gtar "$@"
    elif command -v tar >/dev/null; then
        tar "$@"
    else
        printe_err "No tar program found"
        printe_err "Try install \`bsdtar\` or \`gtar\`"
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


## Fetching
##

git_clone() {
    repo=$1; shift
    path=$1; shift
    ref=$1

    if [ -z "$ref" ]; then
        ref=master
    fi

    if [ ! -d "$path" ]; then
        git clone "$repo" "$path"
        git -C "$path" checkout "$ref"
    elif [ "$(git -C "$path" describe --all 2>&1)" = "heads/$ref" ]; then
        git -C "$path" checkout -q "$ref"
        git -C "$path" pull -q origin "$ref"
        printe_info "$path successfully updated"
    elif [ "$(git -C "$path" describe 2>&1)" != "$ref" ] &&
         [ "$(git -C "$path" rev-parse --short HEAD)" != "$ref" ]; then
        git -C "$path" fetch origin
        git -C "$path" checkout "$ref"
    else
        printe_info "$path already at $ref"
    fi
}

fetch_url() {
    output=$1; shift
    url=$1; shift
    tmpfile=$(mktemp)

    if command -v curl >/dev/null; then
        printe_info "Downloading $url with curl..."
        if ! curl -sSL -o "$tmpfile" "$url"; then
            rm "$tmpfile"
            exit 1
        fi
    elif command -v fetch >/dev/null; then
        printe_info "Downloading $url with fetch..."
        if ! fetch -q -o "$tmpfile" "$url"; then
            rm "$tmpfile"
            exit 1
        fi
    elif command -v wget >/dev/null; then
        printe_info "Downloading $url with wget..."
        if ! wget -q -O "$tmpfile" "$url"; then
            rm "$tmpfile"
            exit 1
        fi
    elif command -v ftp >/dev/null; then
        printe_info "Downloading $url with ftp..."
        if ! ftp -V -o "$tmpfile" "$url"; then
            rm "$tmpfile"
            exit 1
        fi
    else
        printe_err "Cannot fetch URL"
        printe_err "Try installing \`curl\`"
        rm "$tmpfile"
        exit 1
    fi

    if [ "$output" = "-" ]; then
        cat "$tmpfile"
        rm "$tmpfile"
    else
        mv "$tmpfile" "$output"
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

    fetch_url \
        "$output" \
        "https://raw.githubusercontent.com/$gh_repo/$gh_ref/$gh_path"
}


## Sysinfo
##

get_platform() {
    platform=$(uname | tr '[:upper:]' '[:lower:]')

    if [ "$platform" = "linux" ]; then
        if [ -f /etc/os-release ] && grep -q ubuntu /etc/os-release; then
            platform=ubuntu
        elif [ -f /etc/os-release ] && grep -q debian /etc/os-release; then
            platform=debian
        elif [ -f /etc/os-release ] && grep -q void /etc/os-release; then
            platform=void
        elif [ -f /etc/alpine-release ]; then
            platform=alpine
        elif [ -f /etc/arch-release ]; then
            platform=arch
        elif [ -f /etc/artix-release ]; then
            platform=arch
        fi
    fi

    echo "$platform"
}

get_netif() {
    netif=

    case $(get_platform) in
        freebsd )
            for i in $(ifconfig -l -u); do
                if ifconfig "$i" |grep -q ether; then
                    netif=$i
                    break
                fi
            done
            ;;

        * )
            ;;
    esac

    if [ -z "$netif" ]; then
        printe_err "Could not determine primary network interface"
        exit 1
    fi

    echo "$netif"
}

get_sshd_port() {
    sshd_config=/etc/ssh/sshd_config

    if [ ! -f $sshd_config ]; then
        printe_err "sshd configuration could not be found"
        exit 1
    fi

    sshd_port=$(awk '/^#? ?Port/ { print $NF }' < $sshd_config)

    if [ -z "$sshd_port" ]; then
        printe_err "Could not determine sshd port"
        exit 1
    fi

    echo "$sshd_port"
}


## Utilities
##

forced() {
    normalize_bool "$FORCE"
}

verify_shasum() {
    filepath=$1; shift
    shasum=$1; shift
    cmd=

    if command -v sha256 >/dev/null; then
        cmd=sha256
    elif command -v shasum >/dev/null; then
        cmd=shasum
    elif command -v sha256sum >/dev/null; then
        cmd=sha256sum
    fi

    if [ -z "$cmd" ]; then
        printe_info "No sha256 binary found, skipping checksum..."
        return 0
    fi

    echo "$shasum  $filepath" | "$cmd" -c -
}

make_link() {
    OPTIND=1

    command=
    no_check_src=

    while getopts "Sf" opt; do
        case "$opt" in
            S ) command="run_root sh";;
            f ) no_check_src=1;;
            * )
                printe_err "Invalid flags given to make_link"
                exit 1
                ;;
        esac
    done

    shift $((OPTIND-1))

    if [ "${1:-}" = "--" ]; then
        shift
    fi

    src=$1; shift
    dest=$1; shift

    ts=$(date +%s)

    if [ -z "$command" ]; then
        command="sh"
    fi

    if [ "$no_check_src" != 1 ] && [ ! -e "$src" ]; then
        printe_info "$src does not exists, skipping..."
        return
    fi

    if forced && [ -e "$dest" ]; then
        $command <<EOF
mv "$dest" "$dest.dotfiles.$ts"
EOF
    fi

    if [ -f "$dest" ] && [ ! -L "$dest" ]; then
        printe_info "$dest already exists and is not a link, skipping..."
        return
    fi

    if [ "$(readlink "$dest")" = "$src" ]; then
        printe_info "$dest already linked"
        return
    fi

    $command <<EOF
mkdir -p "$(dirname "$dest")"
ln -sf "$src" "$dest"
EOF

    printe_info "$dest linked to $src"
}

lineinfile() {
    OPTIND=1

    file=
    regexp=
    line=
    state=
    command=

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

    shift $((OPTIND-1))

    if [ "${1:-}" = "--" ]; then
        shift
    fi

    if [ -z "$line" ] && [ "$state" != "absent" ]; then
        printe_err "line must be specified unless state is absent"
        exit 1
    fi

    if [ -z "$file" ] || [ ! -f "$file" ]; then
        printe_err "file must be present"
        exit 1
    fi

    if [ -z "$command" ]; then command="sh"; fi
    if [ -z "$state" ]; then state=present; fi
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

change_shell() {
    target_shell=$1; shift

    if ! target_shell_bin=$(command -v "$target_shell"); then
        printe_err "$target_shell is not a valid shell, skipping..."
        return
    fi

    if [ "$SHELL" = "$target_shell_bin" ]; then
        printe_info "Already running $target_shell, skipping..."
        return
    fi

    if ! command -v chsh >/dev/null; then
        printe_err "chsh is not available, skipping..."
        return
    fi

    if ! grep -q "$target_shell_bin" /etc/shells; then
        printe_info "Adding $target_shell_bin to /etc/shells..."
        printf "%s\\n" "$target_shell_bin" | run_root tee -a /etc/shells
    fi

    run_root chsh -s "$target_shell_bin" "$USER"
}


## Runner
##

run_with_flavors() {
    flavors=$*

    if [ "$(command -v _preflight)x" != "x" ]; then
        if ! _preflight "$flavors"; then
            return
        fi
    fi

    if [ "$(command -v _run)x" != "x" ]; then
        if ! _run "$flavors"; then
            return
        fi
    fi

    for flavor in $flavors; do
        run_command="_run_$flavor"

        if [ "$(command -v "$run_command")x" != "x" ]; then
            "$run_command" "$flavors"
        fi
    done
}
