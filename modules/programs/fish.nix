{ config, lib, pkgs, ... }:

let
  inherit (lib) cli escapeShellArg mkIf mkMerge;

  dotfilesDir = "${config.home.homeDirectory}/.dotfiles";
in
{
  programs.fish = {
    enable = true;

    plugins = [
      { name = "tide"; src = pkgs.fishPlugins.tide.src; }
      { name = "sponge"; src = pkgs.fishPlugins.sponge.src; }
    ];

    interactiveShellInit = ''
      set fish_greeting ""

      if test -z $INSIDE_EMACS
        # Display system details on new terminal
        ${pkgs.fastfetch}/bin/fastfetch
      end
    '';

    functions = {
      gg = {
        body = ''
          set -l dir ($HOME/.dotfiles/bin/pom list | fzf --layout reverse --height 40% -q "$argv")
          if not set -q dir
            return
          end
          cd $dir
        '';
      };

      pcd = {
        body = ''
          if not test -d $HOME/Dropbox/Projects
            echo >&2 "No projects directory"
            return 1
          end
          set -l dir (find "$HOME/Dropbox/Projects" -type d -not -iname ".*" | fzf --layout reverse --height 40% -q "$argv")
          if not set -q dir
            return
          end
          cd $dir
        '';
      };
    };
  };

  home.activation = {
    setupTide = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      setupTide() {
        ${config.programs.fish.package}/bin/fish -c ${escapeShellArg "tide configure ${
          cli.toGNUCommandLineShell { } {
            auto = true;
            style = "Lean";
            prompt_colors = "16 colors";
            show_time = "24-hour format";
            lean_prompt_height = "Two lines";
            prompt_connection = "Disconnected";
            prompt_spacing = "Sparse";
            icons = "Few icons";
            transient = "No";
          }
        }"} >/dev/null 2>&1
      }
      setupTide
    '';
  };
}
