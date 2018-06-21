autoload -Uz add-zsh-hook

_window_hook () {
    _window_cwd=$(print -P "%~"| sed 's/\([^[:punct:]]\)[^\/]*\//\1\//g')
    _window_title="\e]0;$_window_cwd\a"
    echo -ne "$_window_title"
}

add-zsh-hook precmd _window_hook
