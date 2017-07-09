if [ -d "$HOME/.pyenv" ]; then
    PATH=$HOME/.pyenv/bin:$PATH
    PATH=$HOME/.pyenv/shims:$PATH
    pyenv rehash &>/dev/null &disown
fi
