function vi
    if which nvim 2>&1 >/dev/null
        command nvim $argv
    else if which vim 2>&1 >/dev/null
        command vim $argv
    else
        command vi $argv
    end
end