alias ls="ls -G"
alias ll="ls -alh"
alias ka="k -A"
alias emacs="emacs -nw"

if hash fzy 2>/dev/null; then
    alias ff="find . -type f |fzy"
    alias fd="find . -type d |fzy"
fi

if hash nvim 2>/dev/null; then
    alias vim="nvim"
fi

if hash hub 2>/dev/null; then
    alias git="hub"
fi
