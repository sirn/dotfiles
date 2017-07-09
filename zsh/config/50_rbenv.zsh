if [ -d "$HOME/.rbenv" ]; then
    PATH=$HOME/.rbenv/bin:$PATH
    PATH=$HOME/.rbenv/shims:$PATH
    rbenv rehash &>/dev/null &disown
fi
