#!/usr/bin/env zsh
DOTFILES_DIR=`cd ${0%/*} && pwd -P`

# Helper function
function _link_to_home {
    local source_path=$DOTFILES_DIR/$1
    local dest_path=$HOME/$2
    if [[ -e $dest_path ]]; then
        echo "* Skipping $2 as it is already exist."
    else
        echo "* Linking $2 to the home directory"
        ln -s $source_path $dest_path
    fi
}

# Installation
_link_to_home zsh/zshrc .zshrc