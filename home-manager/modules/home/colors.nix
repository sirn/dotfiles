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
        "auto"
        "dark"
        "light"
      ];
      default = "auto";
      description = "Default scheme for apps that follow the system: auto, light, or dark.";
    };

    variants = {
      desktop = mkOption {
        type = types.enum [
          "auto"
          "dark"
          "light"
        ];
        default = config.home.colors.variant;
        description = "Default desktop scheme for apps that follow the system: auto, light, or dark.";
      };
      terminal = mkOption {
        type = types.enum [
          "auto"
          "dark"
          "light"
        ];
        default = config.home.colors.variant;
        description = "Default terminal scheme for apps that follow the system: auto, light, or dark.";
      };
      desktopFallback = mkOption {
        type = types.enum [
          "dark"
          "light"
        ];
        default = "light";
        description = "Static desktop variant for apps that do not support auto.";
      };
      terminalFallback = mkOption {
        type = types.enum [
          "dark"
          "light"
        ];
        default = "dark";
        description = "Static terminal variant for apps that do not support auto.";
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
