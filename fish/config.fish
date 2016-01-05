if status --is-login --is-interactive

    # Fish reassigns $fish_greeting if it is not set so we can't do -e.
    set fish_greeting ""

    # Few custom bin paths. .local is machine-specific while .dotfiles is shared
    # between all machines and is version controlled (i.e. this repository).
    set PATH /usr/local/sbin $PATH
    set PATH /usr/local/bin $PATH
    set PATH $HOME/.local/bin $PATH
    set PATH $HOME/.dotfiles/bin $PATH

    # Prefer hub over git for GitHub integration.
    if which hub 2>&1 >/dev/null
       function git; hub $argv; end
    end

    # Prefer nvim over vim over vi.
    if which nvim 2>&1 >/dev/null
        function vim; nvim $argv; end
        function vi; nvim $argv; end
        set -Ux EDITOR nvim
    else if which vim 2>&1 >/dev/null
        function vi; vim $argv; end
        set -Ux EDITOR vim
    else if which vi 2>&1 >/dev/null
        set -Ux EDITOR vi
    end

    # Allow VMWare to be used headlessly using vmrun.
    if test -d /Applications/VMware\ Fusion.app
        function vmrun
            /Applications/VMware\ Fusion.app/Contents/Library/vmrun $argv
        end
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

    # SSH config.d
    function _reload_ssh_config; cat $HOME/.ssh/config.d/* > $HOME/.ssh/config; end
    function ssh; _reload_ssh_config; command ssh $argv; end
    function scp; _reload_ssh_config; command scp $argv; end

end

# Setup the $SSH_AGENT_PID and $SSH_AUTH_SOCK globally in OSX to use ssh-agent
# initialized by keychain command rather than Mac OS X's Keychain.
if status --is-interactive
    if which keychain 2>&1 >/dev/null
        eval (keychain --eval --quiet) >/dev/null
        launchctl setenv SSH_AGENT_PID $SSH_AGENT_PID
        launchctl setenv SSH_AUTH_SOCK $SSH_AUTH_SOCK
    end
end
