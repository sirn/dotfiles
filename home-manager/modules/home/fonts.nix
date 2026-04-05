{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkOption types;
in
{
  options.home.fonts = {
    desktop = {
      sansSerif = mkOption {
        type = types.str;
        default = "Noto Sans";
        description = "Default sans-serif font for desktop applications.";
      };

      serif = mkOption {
        type = types.str;
        default = "Noto Serif";
        description = "Default serif font for desktop applications.";
      };

      monospace = mkOption {
        type = types.str;
        default = "Hack";
        description = "Default monospace font for desktop applications.";
      };
    };

    terminal = {
      monospace = mkOption {
        type = types.str;
        default = config.home.fonts.desktop.monospace;
        defaultText = "config.home.fonts.desktop.monospace";
        description = "Monospace font for terminal emulators.";
      };

      size = mkOption {
        type = types.int;
        default = if pkgs.stdenv.isDarwin then 14 else 12;
        description = "Font size for terminal emulators.";
      };
    };

    editor = {
      monospace = mkOption {
        type = types.str;
        default = config.home.fonts.desktop.monospace;
        defaultText = "config.home.fonts.desktop.monospace";
        description = "Monospace font for text editors.";
      };

      size = mkOption {
        type = types.int;
        default = if pkgs.stdenv.isDarwin then 14 else 12;
        description = "Font size for text editors.";
      };
    };
  };
}
