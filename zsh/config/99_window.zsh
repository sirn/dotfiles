autoload -Uz add-zsh-hook

_window_hook () {
    local _window_title="\e]0;$(pwd)\a"
    printf "$_window_title"
}

add-zsh-hook precmd _window_hook
