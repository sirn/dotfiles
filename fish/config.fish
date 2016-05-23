if status --is-login

    # Fish reassigns $fish_greeting if it is not set so we can't do -e.
    set fish_greeting ""

    set -x LC_ALL en_US.UTF-8
    set -x LANG en_US.UTF-8
    set -x EDITOR vi

    set -x PATH /usr/local/sbin $PATH
    set -x PATH /usr/local/bin $PATH
    set -x PATH $HOME/.local/bin $PATH
    set -x PATH $HOME/.dotfiles/bin $PATH

    if which brew 2>&1 >/dev/null
        set -x HOMEBREW_NO_ANALYTICS 1
        set -x OPENSSL_INCLUDE_DIR /usr/local/opt/openssl/include
        set -x OPENSSL_LIB_DIR /usr/local/opt/openssl/lib
    end

    if test -d $HOME/.rbenv
        set -x PATH $HOME/.rbenv/bin $PATH
        set -x PATH $HOME/.rbenv/shims $PATH
        rbenv rehash >/dev/null
    end

    if test -d $HOME/.pyenv
        set -x PATH $HOME/.pyenv/bin $PATH
        set -x PATH $HOME/.pyenv/shims $PATH
        pyenv rehash >/dev/null
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
