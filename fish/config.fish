if status --is-login

    # Fish reassigns $fish_greeting if it is not set so we can't do -e.
    set fish_greeting ""

    # General bin paths
    set PATH /usr/local/sbin $PATH
    set PATH /usr/local/bin $PATH
    set PATH $HOME/.local/bin $PATH

    # Ruby-specific configurations
    if test -d $HOME/.rbenv
        set PATH $HOME/.rbenv/bin $PATH
        set PATH $HOME/.rbenv/shims $PATH
        rbenv rehash >/dev/null
    end

    # Python-specific configurations
    if test -d $HOME/.virtualenvs
        virtualenv activate default
    end

    # Node-specific configurations
    if test -d /usr/local/share/npm/
        set PATH /usr/local/share/npm/bin $PATH
    end

    # Aliases
    function intellij
        open -b com.jetbrains.intellij
    end

end
