function vi
    if type -P nvim >/dev/null 2>&1
        command nvim $argv
    else if type -P vim >/dev/null 2>&1
        command vim $argv
    else
        command vi $argv
    end
end
