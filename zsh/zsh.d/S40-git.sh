_add_path /usr/local/git

# Prompt
function _prompt_git {
    local branch=$(git symbolic-ref HEAD 2>/dev/null|awk '{sub(/^refs\/heads\//, ""); print}')
    if [ $branch ]; then
        echo " %Ugit:$branch%u"
    fi
}

# vim:ft=zsh
