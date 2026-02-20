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

    initContent =
      let
        # Ensure sessionPath takes precedence over system paths on macOS
        darwinPathFix = lib.optionalString pkgs.stdenv.isDarwin ''
          # On macOS, system paths often get prepended after zshenv runs.
          # Re-ensure our preferred PATH order here in .zshrc
          export PATH="${config.home.profileDirectory}/bin:$PATH"
        '';

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

              dir=$(repoman list | "${fzyCmd}" -q "$*")
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
      in
      ''
        ${darwinPathFix}

        export WORDCHARS="''${WORDCHARS/\//}"
        if [[ $- == *"i"* ]]; then
          export SHELL=${cfg.package}/bin/zsh
        fi

        source "${dirJumpCmd}"

        bindkey -e
      '';
  };
}
