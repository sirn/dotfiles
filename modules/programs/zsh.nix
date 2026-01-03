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
      save = 10000;
      size = 10000;
    };

    initContent =
      let
        dirJumpCmd =
          let
            fzyCmd = lib.getExe pkgs.fzy;

            fdCmd = lib.getExe pkgs.fd;
          in
          pkgs.writeScript "dir-jump-cmd" ''
            # Change to a subdirectory of the current directory.
            gg() {
              local dir

              dir=$("${fdCmd}" --type d . | "${fzyCmd}" -q "$*")
              if [ -z "$dir" ]; then
                  return
              fi

              builtin cd "$dir" || return 1
            }

            # Quickly jump into dev project directory.
            ggd() {
              local dir

              dir=$($HOME/.dotfiles/bin/pom list | "${fzyCmd}" -q "$*")
              if [ -z "$dir" ]; then
                  return
              fi

              builtin cd "$dir" || return 1
            }

            # Change directory in a project directory.
            ggp() {
              local dir

              if [ ! -d "$HOME/Dropbox/Projects" ]; then
                echo >&2 "No projects directory"
                return 1
              fi

              dir=$("${fdCmd}" --type d . "$HOME/Dropbox/Projects" | "${fzyCmd}" -q "$*")
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

              dir=$(printf "%s" "$dirs" | "${fzyCmd}" -q "$*")
              if [ -z "$dir" ]; then
                return
              fi

              builtin cd "$dir" || return 1
            }
          '';

        historySetup =
          let
            awkCmd = lib.getExe pkgs.gawk;

            cutCmd = lib.getExe' pkgs.coreutils "cut";

            fzyCmd = lib.getExe pkgs.fzy;
          in
          pkgs.writeScript "history-setup" ''
            __fzy_history() {
              local _selection

              _selection=$(fc -ln -1 0 | "${cutCmd}" -f3- | ${awkCmd} '!seen[$0]++' | ${fzyCmd} -q "$*")
              if [ "$?" = "0" ] || [ -n "$_selection" ]; then
                BUFFER=''${_selection}
                CURSOR=''${#BUFFER}
              fi

              zle redisplay
            }

            zle -N __fzy_history
            bindkey '^R' __fzy_history
          '';
      in
      ''
        export WORDCHARS="''${WORDCHARS/\//}"
        if [[ $- == *"i"* ]]; then
          export SHELL=${cfg.package}/bin/zsh
        fi

        source "${dirJumpCmd}"

        source "${historySetup}"

        bindkey -e
      '';
  };
}
