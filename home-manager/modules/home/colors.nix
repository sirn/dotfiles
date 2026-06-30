{ config, lib, ... }:

let
  inherit (lib) mkOption types;
in
{
  options.home.colors = {
    themeName = mkOption {
      type = types.str;
      default = "modus";
      description = "Color theme family (e.g. nord, modus).";
    };

    variant = mkOption {
      type = types.enum [
        "dark"
        "light"
      ];
      default = "dark";
      description = "Default variant — determines light or dark theme.";
    };

    variants = {
      desktop = mkOption {
        type = types.enum [
          "dark"
          "light"
        ];
        default = config.home.colors.variant;
        description = "Override variant for desktop UI apps (niri, waybar, mako, etc.).";
      };
      terminal = mkOption {
        type = types.enum [
          "dark"
          "light"
        ];
        default = config.home.colors.variant;
        description = "Override variant for terminal apps (ghostty, wezterm, etc.).";
      };
    };

    # Resolved full theme names (set by colors/default.nix, not by users)
    desktopThemeName = mkOption {
      type = types.str;
      internal = true;
      description = "Resolved full name of the active desktop theme.";
    };
    terminalThemeName = mkOption {
      type = types.str;
      internal = true;
      description = "Resolved full name of the active terminal theme.";
    };
  };
}
