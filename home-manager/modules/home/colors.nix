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

  };
}
