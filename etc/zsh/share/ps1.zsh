#!/usr/bin/env zsh
#
# Sets up user prompt
#
_prompt() {
    if [ "$(id -u)" = "0" ]; then
        print \#
    else
        print \$
    fi
}

_prompt_git() {
    local _branch
    _branch=$(command git rev-parse --abbrev-ref HEAD 2>/dev/null)

    if [ -n "$_branch" ]; then
        command git status --porcelain=v2 2>/dev/null |
            awk \
                -v branch="$_branch" \
                -v bold="%{$(tput bold 2>/dev/null || true)%}" \
                -v reset="%{$(tput sgr0 2>/dev/null || true)%}" \
                -v green="%{$(tput setaf 2 2>/dev/null || true)%}" \
                -v yellow="%{$(tput setaf 3 2>/dev/null || true)%}" \
                '
                BEGIN {
                    changed=0
                    staged=0
                }
                /^1 M./ { staged=1 }
                /^1 .M/ { changed=1 }
                /^[^1]/ { changed=1 }
                END {
                    printf "git:%s%s%s", bold, branch, reset
                    if (staged) { printf "%s%s", green, "*" }
                    if (changed) { printf "%s%s", yellow, "*" }
                    printf "%s ", reset
                }'
    fi
}

_prompt_jj() {
    if ! command jj root >/dev/null 2>&1; then
        return
    fi

    local _spec
    _spec=$(command jj log --ignore-working-copy --no-graph -r @ -T 'separate(":",
        change_id.shortest(),
        if(empty, "0", "1"),
    )')

    local _changeid=${_spec%%:*};
    local _changed=${_spec##${_changeid}:}

    printf "jj:%s%s%s" \
           "$(tput bold 2>/dev/null || true)" \
           "$_changeid" \
           "$(tput sgr0 2>/dev/null || true)"

    if [ "$_changed" = "1" ]; then
        printf "%s*%s" \
               "$(tput setaf 3 2>/dev/null || true)" \
               "$(tput sgr0 2>/dev/null || true)"
    fi

    printf " "
}

_prompt_pwd() {
    pwd | sed "
s|^$HOME|~|;                                    # $HOME -> ~
s|\([^[:punct:]]\)[^/]*/|\1/|g;                 # foo/bar/baz -> f/b/baz
s|^\(././\)././././.*/\(./[^/]*\)$|\1.../\2|g;  # 1/2/3/4/5/6/7/8/9/10 -> 1/.../9/10
"
}

_prompt_hostname() {
    # Some BSDs doesn't support hostname -s, we're doing this manually.
    local short_hostname
    short_hostname=$(hostname)
    printf "%s" "${short_hostname%%.*}"
}

_prompt_last_exit() {
    local last_exit=$?
    local format_code=

    if [ "$last_exit" = 1 ]; then
        format_code=$(tput setab 1 2>/dev/null || true)
    elif [ "$last_exit" != 0 ]; then
        format_code=$(tput setab 3 2>/dev/null || true)
    fi

    if [ -n "$format_code" ]; then
        printf \
            "%s%s%s " \
            "%{$format_code%}" \
            "$last_exit" \
            "%{$(tput sgr0 2>/dev/null || true)%}"
    fi
}

_prompt_nix() {
    local t_bold t_reset t_highlight
    t_bold=$(tput bold 2>/dev/null || true)
    t_reset=$(tput sgr0 2>/dev/null || true)
    t_highlight=$(tput setaf 2 2>/dev/null || true)
    if [ -n "$IN_NIX_SHELL" ]; then
        printf \
            "%s[%s]%s " \
            "%{$t_highlight$t_bold%}" \
            "nix-shell" \
            "%{$t_reset%}"
    fi
}

_prompt_init() {
    setopt promptsubst

    case "$TERM" in
        xterm* | rxvt* | foot* | alacritty*)
            PROMPT="\
$(_prompt_nix)\
$(_prompt_hostname) \
\$(_prompt_pwd) \
\$(_prompt_git)\
\$(_prompt_jj)\
\$(_prompt_last_exit)\
$(_prompt) "
            ;;

        screen*)
            PROMPT="\
$(_prompt_nix)\
\$(_prompt_pwd) \
\$(_prompt_git)\
\$(_prompt_last_exit)\
$(_prompt) "
            ;;

        *)
            PROMPT="\
$(_prompt_nix)\
\$(_prompt_last_exit)\
$(_prompt) "
            ;;
    esac
}

_prompt_init
