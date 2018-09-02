alias ls="ls -G"
alias ll="ls -alh"
alias ka="k -A"
alias emacs="emacsclient -c -a ''"

if hash nvim 2>/dev/null; then
    alias vim="nvim"
fi

if hash hub 2>/dev/null; then
    alias git="hub"
fi

if hash drill 2>/dev/null; then
    alias dig="drill"
fi
