{ config, lib, pkgs, ... }:

{
  programs.fish = {
    enable = true;
    package = pkgs.unstable.fish;

    plugins = [
      { name = "sponge"; src = pkgs.fishPlugins.sponge.src; }
      { name = "autopair"; src = pkgs.fishPlugins.autopair.src; }
    ];

    functions = {
      gg = {
        body = ''
          set -l dir ($HOME/.dotfiles/bin/pom list | fzy -q "$argv")
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
          set -l dir (find "$HOME/Dropbox/Projects" -type d -not -iname ".*" | fzy -q "$argv")
          if not set -q dir
            return
          end
          cd $dir
        '';
      };

      fish_greeting = {
        body = "";
      };

      fish_vcs_prompt = {
        body = ''
          ${if config.programs.jujutsu.enable then ''
            fish_jj_prompt $argv
            or fish_git_prompt $argv
          '' else ''
            fish_git_prompt $argv
          ''} or fish_hg_prompt $argv
          or fish_fossil_prompt $argv
        '';
      };

      pbcopy = lib.mkIf pkgs.stdenv.isLinux {
        body = ''
          if test -n "$WAYLAND_DISPLAY"
            ${pkgs.wl-clipboard}/bin/wl-copy
          else if test -n "$DISPLAY"
            ${pkgs.xclip}/bin/xclip -selection clipboard
          else
            echo >&2 "Error: not desktop?"
            exit 1
          end
        '';
      };

      pbpaste = lib.mkIf pkgs.stdenv.isLinux {
        body = ''
          if test -n "$WAYLAND_DISPLAY"
            ${pkgs.wl-clipboard}/bin/wl-paste
          else if test -n "$DISPLAY"
            ${pkgs.xclip}/bin/xclip -selection clipboard -o
          else
            echo >&2 "Error: not desktop?"
            exit 1
          end
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
