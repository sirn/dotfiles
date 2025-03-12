{ config, lib, pkgs, ... }:

let
  inherit (lib) cli escapeShellArg mkIf mkMerge;

  dotfilesDir = "${config.home.homeDirectory}/.dotfiles";
in
{
  programs.fish = {
    enable = true;

    plugins = [
      { name = "sponge"; src = pkgs.fishPlugins.sponge.src; }
      { name = "autopair"; src = pkgs.fishPlugins.autopair.src; }
    ];

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

    interactiveShellInit = ''
      set -gx SHELL "${config.programs.fish.package}/bin/fish"
      set -g sponge_successful_exit_codes 0
      set -g sponge_allow_previously_successful false
      set -g sponge_delay 10
    '';
  };
}
