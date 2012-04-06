if [[ "$TERM" == "screen" ]]; then
    # Show current command in screen session
    function preexec {
        local CMD=${1[(wr)^(*=*|sudo|-*)]}
        echo -ne "\ek$CMD\e\\"
    }
fi

# vim:ft=zsh
