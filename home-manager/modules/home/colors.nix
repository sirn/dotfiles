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
  options.home.colors = {
    themeName = mkOption {
      type = types.str;
      default = "modus-vivendi";
      description = "Name of the color theme to use by default.";
    };

    variant = mkOption {
      type = types.enum [
        "dark"
        "light"
      ];
      default = "dark";
      description = "Theme variant - determines if theme is dark or light.";
    };

    emacsTheme = {
      packages = mkOption {
        type = types.functionTo (types.listOf types.package);
        default = epkgs: [ ];
        description = "Emacs packages required for this theme.";
      };

      customElisp = mkOption {
        type = types.str;
        default = "";
        description = "Custom Elisp to load and configure the Emacs theme.";
      };
    };
  };
}
