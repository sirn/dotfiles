# ZSH Path
fpath=(~/.dotfiles/zsh/functions $fpath)

# Enable colors for color-supported terms
autoload colors
zmodload zsh/terminfo
if [ "$terminfo[colors]" -ge 8 ]; then
    colors
    export COLORTERM=yes
fi

# Paths
export PATH="/usr/local/bin:/opt/local/bin:/sbin"
export PATH="$PATH:/bin:/usr/sbin:/usr/bin:/usr/local/sbin"
export LC_ALL="en_US.UTF-8"
export LANG="en_US.UTF-8"

export EDITOR='vim'

if [ $is_bsd ]; then
  # I won't use GNU tools in BSD anyway
  export LSCOLORS="ExGxFxdxCxDxDxhbadExEx"
  export CLICOLOR="yes"
fi

# Completion
autoload -U compinit
compinit
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache
zstyle ':completion:*' completer _complete _match _approximate _expand
zstyle ':completion:*' insert-unambiguous false

setopt extendedglob
setopt auto_cd
setopt correctall

# vim:ft=zsh
