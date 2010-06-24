if [[ "$TERM" == "screen" ]]; then
    
    # Set screen's status
    function preexec {
        local CMD=${1[(wr)^(*=*|sudo|-*)]}
        echo -ne "\ek$CMD\e\\"
    }
    
fi

# vim:ft=zsh
