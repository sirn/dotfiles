if status --is-login
    set fish_greeting ""

    set -x LC_ALL en_US.UTF-8
    set -x LANG en_US.UTF-8
    set -x EDITOR vi

    set -x PATH /usr/local/sbin $PATH
    set -x PATH /usr/local/bin $PATH
    set -x PATH $HOME/.local/bin $PATH
    set -x PATH $HOME/.dotfiles/bin $PATH

    if type -P brew >/dev/null 2>&1
        set -x HOMEBREW_NO_ANALYTICS 1
        set -x OPENSSL_INCLUDE_DIR /usr/local/opt/openssl/include
        set -x OPENSSL_LIB_DIR /usr/local/opt/openssl/lib
    end

    if type -P cargo >/dev/null 2>&1
        set -x PATH $HOME/.cargo/bin $PATH
        set -x RUST_SRC_PATH $HOME/.local/src/rust/current/src/
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

    if test -f /usr/libexec/java_home
        set -x JAVA_HOME (/usr/libexec/java_home)
    end

    if type -P keychain >/dev/null 2>&1
        eval (keychain --agents ssh,gpg --eval --quiet) >/dev/null
    end

    set google_cloud_path /usr/local/Caskroom/google-cloud-sdk
    if test -d $google_cloud_path
        set -x PATH $google_cloud_path/latest/google-cloud-sdk/bin $PATH
    end
end

if status --is-interactive
    if type -P direnv >/dev/null 2>&1
       eval (direnv hook fish)
    end
end
