# Enable colors for color-supported terms
autoload colors
zmodload zsh/terminfo
if [[ "$terminfo[colors]" -ge 8 ]]; then
    colors
    export COLORTERM=yes
fi

# Enable completion
autoload -U compinit
mkdir -p ~/.zsh # Make sure zcompdump could be created
compinit -d ~/.zsh/zcompdump
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path ~/.zsh/cache
zstyle ':completion:*' completer _complete _match _approximate _expand
zstyle ':completion:*' insert-unambiguous false

# ZSH Options
setopt zle            # Enable ZSH line editor
setopt auto_list      # List ambiguous by default
setopt auto_menu      # Show ambiguous listing menu by default
setopt hash_cmds      # Place location of each command inside hash table
setopt hash_dirs      # Place directory containing command inside hash table
setopt correct        # Try to correct the spelling of ocmmands
setopt extended_glob  # Perform filename generation
setopt auto_cd        # Perform cd to the directory if no command exist
setopt auto_param_keys   # Append }, : to params automatically, if needed
setopt auto_param_slash  # Append trailing slash for parameters
setopt auto_remove_slash # Remove slash if the next character is delimiter
unsetopt beep         # Disable annoying BEEP
unsetopt bg_nice      # Don't NICE the background process

# vim:ft=zsh