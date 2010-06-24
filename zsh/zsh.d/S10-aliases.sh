if [[ -z $is_bsd ]]; then
    # Not BSD, assume it's using GNU toolchain
    alias ls="ls --color=auto"
fi

alias vi='vim'
alias ll='ls -l'
alias la='ls -A'
alias lla='ls -alh'
alias l='ls -CF'

alias mv='nocorrect mv'
alias cp='nocorrect cp'
alias ln='nocorrect ln'
alias mkdir='nocorrect mkdir'

alias git-serve='git daemon --reuseaddr --export-all --base-path=. --enable=receive-pack .'

# vim:ft=zsh