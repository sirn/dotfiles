{
  config,
  lib,
  pkgs,
  ...
}:

let
  tomlFormat = pkgs.formats.toml { };

  tmuxcfg = config.programs.tmux;

  fzyCmd = lib.getExe pkgs.fzy;
in
{
  home.packages = with pkgs; [ local.repoman ];

  xdg.configFile."repoman/config.toml".source = tomlFormat.generate "repoman-config" {
    roots = [
      "${config.home.homeDirectory}/Dev/src"
      "${config.home.homeDirectory}/Dev/go/gopath/src"
    ];
    workspaces = [
      "${config.home.homeDirectory}/Dev/workspace"
      "${config.home.homeDirectory}/Dev/adhoc"
    ];
  };

  programs.zsh.initContent = lib.mkIf config.programs.zsh.enable ''
    # Quickly jump into dev project directory.
    ggd() {
      local dir

      dir=$(repoman list | "${fzyCmd}" -q "$*")
      if [ -z "$dir" ]; then
          return
      fi

      builtin cd "$dir" || return 1
    }

    # Quickly jump into workspace directory.
    ggw() {
      local dir

      dir=$(repoman workspace list | "${fzyCmd}" -q "$*")
      if [ -z "$dir" ]; then
          return
      fi

      builtin cd "$dir" || return 1
    }

    ${lib.optionalString tmuxcfg.enable ''
      # Open workspace in a tmux session.
      # Use exec when outside tmux so detaching closes the terminal.
      # Don't exec when inside tmux or the pane will close on switch.
      #
      # Note: tmux_init has matching $TMUX logic - it uses switch-client when
      # inside tmux (exits immediately) and new-session when outside (blocks).
      #
      # We need the conditional exec in both places to get the right behavior:
      # - Outside tmux: exec tmux_init → exec new-session → terminal closes on detach
      # - Inside tmux: run tmux_init → switch-client → return to shell, pane stays open
      #
      ggt() {
        local dir name

        dir=$(repoman workspace list | "${fzyCmd}" -q "$*")
        if [ -z "$dir" ]; then
            return
        fi

        name=$(basename "$dir")
        if [ -n "$TMUX" ]; then
          "$HOME/.tmux_init" "$name" "$dir"
        else
          exec "$HOME/.tmux_init" "$name" "$dir"
        fi
      }
    ''}
  '';

  programs.fish.functions = lib.mkIf config.programs.fish.enable (
    {
      ggd = {
        body = ''
          set -l dir (repoman list | ${fzyCmd} -q "$argv")
          if test -z "$dir"
            return
          end
          cd $dir
        '';
      };

      ggw = {
        body = ''
          set -l dir (repoman workspace list | ${fzyCmd} -q "$argv")
          if test -z "$dir"
            return
          end
          cd $dir
        '';
      };
    }
    // lib.optionalAttrs tmuxcfg.enable {
      ggt = {
        body = ''
          # Open workspace in a tmux session.
          #
          # Use exec when outside tmux so detaching closes the terminal.
          # Don't exec when inside tmux or the pane will close on switch.
          #
          # Note: tmux_init has matching $TMUX logic - it uses switch-client when
          # inside tmux (exits immediately) and new-session when outside (blocks).
          #
          # We need the conditional exec in both places to get the right behavior:
          # - Outside tmux: exec tmux_init → exec new-session → terminal closes on detach
          # - Inside tmux: run tmux_init → switch-client → return to shell, pane stays open
          #
          set -l dir (repoman workspace list | ${fzyCmd} -q "$argv")
          if test -z "$dir"
            return
          end
          set -l name (basename $dir)
          if set -q TMUX
            $HOME/.tmux_init $name $dir
          else
            exec $HOME/.tmux_init $name $dir
          end
        '';
      };
    }
  );
}
