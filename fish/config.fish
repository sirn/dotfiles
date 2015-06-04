if status --is-login

    # Fish reassigns $fish_greeting if it is not set so we can't do -e.
    set fish_greeting ""

    # General bin paths.
    set PATH /usr/local/sbin $PATH
    set PATH /usr/local/bin $PATH
    set PATH $HOME/.local/bin $PATH
    set PATH $HOME/.dotfiles/bin $PATH

    # Use VIM as editor if available.
    if which vim 2>&1 >/dev/null
        set -Ux EDITOR vim
        function vi; vim $argv; end
    end

    # Ruby-specific configurations.
    if test -d $HOME/.rbenv
        set PATH $HOME/.rbenv/bin $PATH
        set PATH $HOME/.rbenv/shims $PATH
        rbenv rehash >/dev/null
    end

    # Python-specific configurations.
    if test -d $HOME/.pyenv
        set PATH $HOME/.pyenv/bin $PATH
        set PATH $HOME/.pyenv/shims $PATH
        pyenv rehash >/dev/null
    end

    # Node-specific configurations.
    if test -d /usr/local/share/npm/
        set PATH /usr/local/share/npm/bin $PATH
    end

    # Directory bin.
    set PATH ./bin $PATH

    # Headless VMWare.
    if test -d /Applications/VMware\ Fusion.app
        function vmrun
            /Applications/VMware\ Fusion.app/Contents/Library/vmrun $argv
        end
    end

    # Aliases.
    function intellij; open -b com.jetbrains.intellij $argv; end
    if which hub 2>&1 >/dev/null
        function git; hub $argv; end
    end

end

if status --is-interactive
    if which keychain 2>&1 >/dev/null
        eval (keychain --eval --quiet) >/dev/null
    end
end
