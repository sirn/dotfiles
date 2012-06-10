if status --is-login

    # General bin paths
    set PATH /usr/local/sbin $PATH
    set PATH /usr/local/bin $PATH
    set PATH ~/.local/bin $PATH

    # Load config.d files
    for fish_snipplet in ~/.dotfiles/fish/config.d/S*
        . $fish_snipplet
    end

end
