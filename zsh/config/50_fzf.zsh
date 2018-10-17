case "$(uname)" in
    FreeBSD) FZF_PATH="/usr/local/share/examples/fzf/shell";;
    Darwin)  FZF_PATH="/usr/local/opt/fzf/shell";;
esac

if [ "$FZF_PATH" && -d "$FZF_PATH" ]; then
    source "$FZF_PATH/key-bindings.zsh"
    source "$FZF_PATH/completion.zsh"
fi
