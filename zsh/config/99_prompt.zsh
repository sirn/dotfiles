#!/usr/local/bin/env zsh

case "$TERM" in
    xterm* | rxvt* | eterm* | screen* )
        autoload -Uz add-zsh-hook
        autoload -Uz vcs_info

        setopt prompt_subst
        bindkey -e

        zstyle ':vcs_info:*' stagedstr '%F{green}*'
        zstyle ':vcs_info:*' unstagedstr '%F{yellow}*'
        zstyle ':vcs_info:*' check-for-changes true
        zstyle ':vcs_info:*' enable git svn
        zstyle ':vcs_info:*' formats ' %b%c%u'
        zstyle ':vcs_info:*' actionformats ' %b:%a%c%u'

        _prompt_hook () {
            _prompt_cwd=$(print -P "%~"| sed 's/\([^[:punct:]]\)[^\/]*\//\1\//g')
            vcs_info
        }

        add-zsh-hook precmd _prompt_hook

        HISTFILE=~/.zhistory
        HISTSIZE=10000
        SAVEHIST=10000

        PROMPT_HOSTNAME='%m'
        PROMPT_CWD=' ${_prompt_cwd}'
        PROMPT_GIT='${vcs_info_msg_0_}%{$reset_color%}'
        PROMPT="%{$reset_color%}${PROMPT_HOSTNAME}${PROMPT_CWD}${PROMPT_GIT}%{$reset_color%}"

        if [ -f "${HOME}/.local/src/kube-ps1/kube-ps1.sh" ]; then
            source "${HOME}/.local/src/kube-ps1/kube-ps1.sh"
            KUBE_PS1_CTX_COLOR=null
            KUBE_PS1_NS_COLOR=null
            KUBE_PS1_SEPARATOR=""
            KUBE_PS1_DIVIDER="/"
            KUBE_PS1_PREFIX=" ["
            KUBE_PS1_SUFFIX="]"
            PROMPT="${PROMPT}"'$(kube_ps1)'
        fi

        PROMPT="${PROMPT} $ "
        ;;

    * )
        PROMPT="$ "
        ;;
esac
