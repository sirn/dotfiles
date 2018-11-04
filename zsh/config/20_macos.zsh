autoload -Uz add-zsh-hook

if [ $TERM_PROGRAM == "Apple_Terminal" ] && [ -z $INSIDE_EMACS ]; then
    _prompt_hook_path () {
        local file_url="file://$HOSTNAME${PWD// /%20}"
        printf '\e]7;%s\a' "$file_url"
    }

    add-zsh-hook precmd _prompt_hook_path
fi
