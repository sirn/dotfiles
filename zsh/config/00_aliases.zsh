alias ls="ls -G"
alias ll="ls -alh"
alias emacs="emacs -nw"

if hash nvim 2>/dev/null; then
    alias vim="nvim"
fi

if hash hub 2>/dev/null; then
    alias git="hub"
fi
