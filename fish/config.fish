if status --is-login
    set fish_greeting ""

    set -x LC_ALL en_US.UTF-8
    set -x LANG en_US.UTF-8
    set -x EDITOR vi

    set -x PATH /usr/local/sbin $PATH
    set -x PATH /usr/local/bin $PATH
    set -x PATH $HOME/.local/bin $PATH
    set -x PATH $HOME/.dotfiles/bin $PATH

    if test -d $HOME/.nix-profile/
        eval (eval $HOME/.dotfiles/bin/nix-fish-env) 2>/dev/null
        set -x NIXPKGS_CONFIG $HOME/.dotfiles/nix/default.nix
        set -x GIT_SSL_CAINFO $SSL_CERT_FILE
    end

    if which keychain 2>&1 >/dev/null
        eval (keychain --eval --quiet) >/dev/null
    end
end

if status --is-interactive
    if which direnv 2>&1 >/dev/null
       eval (direnv hook fish)
    end
end
