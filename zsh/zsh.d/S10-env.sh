# Locales
if [[ -n $IS_BSD ]]; then
  export LC_ALL="en_US.UTF-8"
  export LC_CTYPE="en_US.UTF-8"
  export LANG="en_US.UTF-8"
fi

# Personalize
export EDITOR='vim'

if [[ -n $IS_BSD ]]; then
    # I won't use GNU tools in BSD anyway
    export LSCOLORS="ExGxFxdxCxDxDxhbadExEx"
    export CLICOLOR="yes"
else
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
alias git-diff-mate='git diff --no-color |mate'
alias psmem='ps -o rss,ucomm'
alias lsnet='lsof -r -i TCP -i UDP'

# vim:ft=zsh
