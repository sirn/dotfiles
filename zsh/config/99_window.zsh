autoload -Uz add-zsh-hook

_window_hook () {
    _window_title="\e]0;$(PWD)\a"
    echo -ne "$_window_title"
}

add-zsh-hook precmd _window_hook
