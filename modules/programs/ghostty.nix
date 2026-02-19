{ lib, config, pkgs, ... }:

let
  cfg = config.programs.ghostty;

  swaycfg = config.wayland.windowManager.sway;

  niricfg = config.programs.niri;

  fuzzelcfg = config.programs.fuzzel;

  ghosttyLauncher = config.lib.machine.wrapLauncher cfg.package;
in
{
  programs.ghostty = {
    enable = true;

    package =
      if pkgs.stdenv.isLinux
      then config.lib.nixGL.wrap pkgs.ghostty
      else pkgs.ghostty-bin;

    settings = lib.mkMerge [
      {
        font-family = "PragmataPro Mono Liga";

        # Ctrl+[ traditionally sends ESC; fixterm disambiguates it,
        # breaking Evil insert-mode escape in Emacs.
        keybind = [ "ctrl+bracket_left=text:\\x1b" ];

        command = builtins.concatStringsSep " " (
          [ config.machine.interactiveShell ]
          ++ lib.optional pkgs.stdenv.isDarwin "--login"
        );
      }
      (lib.mkIf pkgs.stdenv.isLinux {
        font-size = 12;

        window-theme = "auto";

        # Disable FreeType hinting so glyphs land at correct subpixel positions
        # under fractional scaling (e.g. 1.25x, 1.5x), preventing jerky rendering.
        freetype-load-flags = "no-hinting";

        # Use compositor server-side decorations to avoid libadwaita CSD artifacts
        # at fractional scales on Wayland.
        window-decoration = "server";
      })
      (lib.mkIf pkgs.stdenv.isDarwin {
        font-size = 14;

        window-theme = "auto";

        macos-titlebar-style = "tabs";

        macos-option-as-alt = true;
      })
    ];
  };

  wayland.windowManager.sway = lib.mkIf (cfg.enable && swaycfg.enable) {
    config = {
      terminal = ghosttyLauncher;
      keybindings = {
        "${swaycfg.config.modifier}+Return" = "exec ${ghosttyLauncher}";
      };
    };
  };

  programs.niri = lib.mkIf (cfg.enable && niricfg.enable) {
    settings = {
      binds = {
        "Mod+T".action.spawn = [
          "${ghosttyLauncher}"
        ];
      };
    };
  };

  programs.fuzzel = lib.mkIf (cfg.enable && fuzzelcfg.enable) {
    settings = {
      main = {
        terminal = lib.getExe cfg.package;
      };
    };
  };
}
