if [ -f "$HOME/.terminfo/78/xterm-24bits" ] && [ -z "$TMUX" ]; then
    TERMINFO="$HOME/.terminfo"
    TERM=xterm-24bits
fi
