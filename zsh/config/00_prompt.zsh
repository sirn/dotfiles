autoload -Uz add-zsh-hook
autoload -Uz vcs_info

setopt prompt_subst
bindkey -e

zstyle ':vcs_info:*' stagedstr '%F{green}*'
zstyle ':vcs_info:*' unstagedstr '%F{yellow}*'
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' enable git svn
zstyle ':vcs_info:*' formats ' %B%b%c%u'
zstyle ':vcs_info:*' actionformats ' %B%b:%a%c%u'

_prompt_hook () {
    _prompt_cwd=$(print -P "%~"| sed 's/\([^[:punct:]]\)[^\/]*\//\1\//g')
    vcs_info
}

add-zsh-hook precmd _prompt_hook

HISTFILE=~/.zhistory
SAVEHIST=100

PROMPT_HOSTNAME='%{${FG[246]}%}%m%{$reset_color%}'
PROMPT_CWD='%{${FG[002]}%} ${_prompt_cwd}%{$reset_color%}'
PROMPT_GIT='${vcs_info_msg_0_}%{$reset_color%}'
PROMPT="${PROMPT_HOSTNAME}${PROMPT_CWD}${PROMPT_GIT} » "
