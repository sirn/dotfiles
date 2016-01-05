#!/usr/bin/env bash
DOTFILES_DIR=`cd ${0%/*} && pwd -P`

# Helper function
function _link_to_home {
    local source_path=$DOTFILES_DIR/$1
    local dest_path=$HOME/$2
    if [[ -e $dest_path ]]; then
        echo "* Skipping $2 as it is already exist."
    else
        echo "* Linking $2 to the home directory"
        mkdir -p `dirname $dest_path` 2>/dev/null
        ln -s $source_path $dest_path
    fi
}

# Installation
_link_to_home emacs/spacemacs .spacemacs
_link_to_home vim/vimperatorrc .vimperatorrc
_link_to_home etc/ackrc .ackrc
_link_to_home etc/amethyst .amethyst
_link_to_home etc/gitconfig .gitconfig
_link_to_home etc/tmux.conf .tmux.conf
_link_to_home etc/aria2.conf .aria2/aria2.conf
_link_to_home ssh/config.d .ssh/config.d
_link_to_home fish/functions .config/fish/functions
_link_to_home fish/config.fish .config/fish/config.fish
