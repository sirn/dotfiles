#!/usr/bin/env zsh
#
# Jump to the repository or clone it to a local directory.
#
gq() {
    local repo
    local repo_c
    local repodir
    local repotype
    local codedirs

    repo=$1
    if [ -z "$repo" ]; then
        printf >&2 "Usage: gq REPO\\n"
        return 2
    fi

    codedirs=()
    if command -v git >/dev/null; then
        for s in $(git config --get-all ghq.root); do
            s=$(
                eval -- builtin cd "$s" 2>/dev/null || return 1
                pwd -P
            )
            if [ -n "$s" ]; then
                codedirs+=("${s}")
            fi
        done
    fi

    if [ "${#codedirs[*]}" -lt 1 ]; then
        codedirs+=($HOME/src)
    fi

    case "$repo" in
    *.git) repotype=git ;;
    *.hg) repotype=hg ;;
    *//hg.* | hg.*) repotype=hg ;;
    *bitbucket.com*) repotype=hg ;;
    *bitbucket.org*) repotype=hg ;;
    *) repotype=git ;;
    esac

    case "$repo" in
    *"://"*) ;;
    *"@"*) ;;
    *)
        repo=https://$repo
        ;;
    esac

    repo_c=$(echo "$repo" |
        sed -E '
                       s|^.*://||;
                       s|^.*@||;
                       s|:[0-9]+/|/|;
                       s|:|/|;
                       s|.hg$||;
                       s|.git$||')

    for s in $codedirs; do
        if [ -d "$s/$repo_c" ]; then
            builtin cd "$s/$repo_c" || return 1
            return 0
        fi
    done

    repodir=${codedirs[1]}/$repo_c

    (
        local st
        local basedir
        local basedir_p

        st=0
        basedir=$(dirname "$repodir")

        mkdir -p "$basedir"
        cd "$basedir" || return 1

        case "$repotype" in
        git) git clone --recurse-submodules "$repo" || st=1 ;;
        hg) hg clone "$repo" || st=1 ;;
        esac

        # Cleanup in case cloning failed; we cannot use trap here
        # since git/hg may return non-standard exit codes.
        if [ "$st" = "1" ]; then
            basedir=$repodir
            while [ "$basedir" != "${codedirs[1]}" ]; do
                rmdir "$basedir" 2>/dev/null || true
                basedir_p=${basedir##*/}
                basedir=${basedir%%/"$basedir_p"}
            done
        fi

        return $st
    ) || return 1

    builtin cd "$repodir" || return 1
}
