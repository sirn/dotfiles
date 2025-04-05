{ config, lib, pkgs, ... }:

{
  programs.zsh = {
    enable = true;
    enableVteIntegration = true;

    syntaxHighlighting = {
      enable = true;
    };

    autocd = true;
    history = {
      append = true;
      expireDuplicatesFirst = true;
      ignoreAllDups = true;
      ignoreDups = true;
      save = 1000;
      size = 1000;
    };

    initExtra = ''
      export WORDCHARS="''${WORDCHARS/\//}"
      if [[ $- == *"i"* ]]; then
        export SHELL=${config.programs.zsh.package}/bin/zsh
      fi

      # Quickly jump into project directory.
      gg() {
        local dir
        dir=$($HOME/.dotfiles/bin/pom list | fzf --layout reverse --height 40% -q "$*")
        if [ -z "$dir" ]; then
            return
        fi
        builtin cd "$dir" || return 1
      }

      # Change directory in a project directory.
      pcd() {
        if [ ! -d "$HOME/Dropbox/Projects" ]; then
          echo >&2 "No projects directory"
          return 1
        fi

        local dir
        dir=$(find "$HOME/Dropbox/Projects" -type d -not -iname '.*' | fzf --layout reverse --height 40% -q "$*")
        builtin cd "$dir" || return 1
      }
    '';
  };
}
