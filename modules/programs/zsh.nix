{ config, lib, pkgs, ... }:

let
  cfg = config.programs.zsh;
in
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
        export SHELL=${cfg.package}/bin/zsh
      fi

      # Quickly jump into project directory.
      gg() {
        local dir
        dir=$($HOME/.dotfiles/bin/pom list | ${pkgs.fzy}/bin/fzy -q "$*")
        if [ -z "$dir" ]; then
            return
        fi
        builtin cd "$dir" || return 1
      }

      # Change directory in a project directory.
      ggp() {
        if [ ! -d "$HOME/Dropbox/Projects" ]; then
          echo >&2 "No projects directory"
          return 1
        fi

        local dir
        dir=$(${pkgs.fd}/bin/fd --type d . "$HOME/Dropbox/Projects" | ${pkgs.fzy}/bin/fzy -q "$*")
        if [ -z "$dir" ]; then
            return
        fi
        builtin cd "$dir" || return 1
      }

      # Change to a subdirectory of the current directory.
      ggd() {
        local dir
        dir=$(${pkgs.fd}/bin/fd --type d . | ${pkgs.fzy}/bin/fzy -q "$*")
        if [ -z "$dir" ]; then
            return
        fi
        builtin cd "$dir" || return 1
      }

      # Navigate to ancestor directories.
      bb() {
        local current_dir dirs dir
        current_dir=$(pwd)
        dirs=""
        while [ "$current_dir" != "/" ]; do
          current_dir=$(dirname "$current_dir")
          dirs="$dirs$current_dir"$'\n'
        done
        if [ -z "$dirs" ]; then
          return
        fi
        dir=$(printf "%s" "$dirs" | ${pkgs.fzy}/bin/fzy -q "$*")
        if [ -z "$dir" ]; then
          return
        fi
        builtin cd "$dir" || return 1
      }
    '';
  };
}
