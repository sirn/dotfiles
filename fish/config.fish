if status --is-login

    # Fish reassigns $fish_greeting if it is not set so we can't do -e.
    set fish_greeting ""

    # General bin paths
    set PATH /usr/local/sbin $PATH
    set PATH /usr/local/bin $PATH
    set PATH ~/.local/bin $PATH

    # Python-specific configurations
    virtualenv activate default

end
