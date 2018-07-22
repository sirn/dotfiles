alias ls="ls -G"
alias ll="ls -alh"
alias ka="k -A"
alias emacs="emacsclient -c -a ''"

if hash fzf 2>/dev/null; then
    alias ff="find . -type f |fzf"
    alias fd="find . -type d |fzf"
fi

if hash nvim 2>/dev/null; then
    alias vim="nvim"
fi

if hash hub 2>/dev/null; then
    alias git="hub"
fi
