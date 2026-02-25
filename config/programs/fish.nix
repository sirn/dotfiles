{ config, lib, pkgs, ... }:

let
  cfg = config.programs.fish;
in
{
  programs.fish = {
    enable = true;

    plugins = [
      { name = "autopair"; src = pkgs.fishPlugins.autopair.src; }
    ];

    functions = {
      gg = {
        body = ''
          set -l dir (${pkgs.fd}/bin/fd --type d . | ${pkgs.fzy}/bin/fzy -q "$argv")
          if test -z "$dir"
            return
          end
          cd $dir
        '';
      };

      ggd = {
        body = ''
          set -l dir (repoman list | ${pkgs.fzy}/bin/fzy -q "$argv")
          if test -z "$dir"
            return
          end
          cd $dir
        '';
      };

      ggp = {
        body = ''
          if not test -d $HOME/Dropbox/Projects
            echo >&2 "No projects directory"
            return 1
          end
          set -l dir (${pkgs.fd}/bin/fd --type d . "$HOME/Dropbox/Projects" | ${pkgs.fzy}/bin/fzy -q "$argv")
          if test -z "$dir"
            return
          end
          cd $dir
        '';
      };

      bb = {
        body = ''
          set -l current_dir (pwd)
          set -l dirs
          while test "$current_dir" != "/"
            set current_dir (dirname "$current_dir")
            set dirs $dirs "$current_dir"
          end
          if test (count $dirs) -eq 0
            return
          end
          set -l dir (printf "%s\n" $dirs | ${pkgs.fzy}/bin/fzy -q "$argv")
          if test -z "$dir"
            return
          end
          cd $dir
        '';
      };

      fish_greeting = {
        body = "";
      };
    };

    interactiveShellInit = ''
      set -gx SHELL "${cfg.package}/bin/fish"
    '';
  };
}
