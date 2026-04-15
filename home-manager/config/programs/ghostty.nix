{
  lib,
  config,
  pkgs,
  ...
}:

let
  cfg = config.programs.ghostty;

  swaycfg = config.wayland.windowManager.sway;

  niricfg = config.programs.niri;

  fuzzelcfg = config.programs.fuzzel;

  fontcfg = config.home.fonts;

  ghosttyLauncher = config.lib.home.wrapLauncher cfg.package;
in
{
  programs.ghostty = {
    enable = true;

    package =
      if pkgs.stdenv.isLinux then
        config.lib.nixGL.wrap pkgs.unstable.ghostty
      else
        pkgs.unstable.ghostty-bin;

    settings = lib.mkMerge [
      {
        font-family = fontcfg.terminal.monospace;

        # Ctrl+[ traditionally sends ESC; fixterm disambiguates it,
        # breaking Evil insert-mode escape in Emacs.
        keybind = [ "ctrl+bracket_left=text:\\x1b" ];

        command = builtins.concatStringsSep " " (
          [ config.home.shell.interactiveShell ] ++ lib.optional pkgs.stdenv.isDarwin "--login"
        );
      }
      (lib.mkIf pkgs.stdenv.isLinux {
        font-size = fontcfg.terminal.size;

        window-theme = "auto";

        # Disable FreeType hinting so glyphs land at correct subpixel positions
        # under fractional scaling (e.g. 1.25x, 1.5x), preventing jerky rendering.
        freetype-load-flags = "no-hinting";

        # Disable "Copied to clipboard" pill notification (GTK only).
        app-notifications = "no-clipboard-copy";

        # Use compositor server-side decorations to avoid libadwaita CSD artifacts
        # at fractional scales on Wayland.
        window-decoration = "server";
      })
      (lib.mkIf pkgs.stdenv.isDarwin {
        font-size = fontcfg.terminal.size;

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
        "Mod+T".action.spawn = [ "${ghosttyLauncher}" ];
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
