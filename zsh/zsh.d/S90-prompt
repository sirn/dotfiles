function _prompt_pwd {
    echo " %B$(print -P "%~"|sed 's/\([^\/]\)[^\/]*\//\1\//g')%b"
}

function precmd {
    
    export PROMPT="%B%m%b %n%# "
    export RPROMPT="$(_prompt_git)$(_prompt_pwd)$(_prompt_virtualenv)"
    
    # Override prompt if running in a screen session
    if [[ "$TERM" == "screen" ]]; then
        export PROMPT="%# "
    fi
    
}

# vim:ft=zsh
