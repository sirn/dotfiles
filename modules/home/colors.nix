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
      description = "Name of the Emacs theme to load.";
    };

    variant = mkOption {
      type = types.enum [
        "dark"
        "light"
      ];
      default = "dark";
      description = "Theme variant - determines if theme is dark or light.";
    };
  };
}
