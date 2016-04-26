if status --is-login --is-interactive

    # Fish reassigns $fish_greeting if it is not set so we can't do -e.
    set fish_greeting ""

    # Few custom bin paths. .local is machine-specific while .dotfiles is shared
    # between all machines and is version controlled (i.e. this repository).
    set -x PATH /usr/local/sbin $PATH
    set -x PATH /usr/local/bin $PATH
    set -x PATH $HOME/.local/bin $PATH
    set -x PATH $HOME/.dotfiles/bin $PATH

    # Locale
    set -x LC_ALL en_US.UTF-8
    set -x LANG en_US.UTF-8

    # Nix
    if test -d $HOME/.nix-profile/
        eval (eval $HOME/.dotfiles/bin/nix-fish-env 2>/dev/null)
        set -x GIT_SSL_CAINFO $SSL_CERT_FILE
    end

    # Enable direnv so we can directory-specific envs in .envrc.
    if which direnv 2>&1 >/dev/null
       eval (direnv hook fish)
    end

    # Prefer nvim over vim over vi.
    if which nvim 2>&1 >/dev/null
        function vim; nvim $argv; end
        function vi; nvim $argv; end
        set -x EDITOR nvim
    else if which vim 2>&1 >/dev/null
        function vi; vim $argv; end
        set -x EDITOR vim
    else if which vi 2>&1 >/dev/null
        set -x EDITOR vi
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
