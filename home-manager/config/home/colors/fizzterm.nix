{
  schemes,
  familyName,
  config,
  lib,
  ...
}:

let
  toPalette = variant: {
    foreground = variant.base16Colors.foreground;
    background = variant.base16Colors.background;
    black = variant.base16Colors.normal.black;
    red = variant.base16Colors.normal.red;
    green = variant.base16Colors.normal.green;
    yellow = variant.base16Colors.normal.yellow;
    blue = variant.base16Colors.normal.blue;
    magenta = variant.base16Colors.normal.magenta;
    cyan = variant.base16Colors.normal.cyan;
    white = variant.base16Colors.normal.white;
    bright_black = variant.base16Colors.bright.black;
    bright_red = variant.base16Colors.bright.red;
    bright_green = variant.base16Colors.bright.green;
    bright_yellow = variant.base16Colors.bright.yellow;
    bright_blue = variant.base16Colors.bright.blue;
    bright_magenta = variant.base16Colors.bright.magenta;
    bright_cyan = variant.base16Colors.bright.cyan;
    bright_white = variant.base16Colors.bright.white;
  };

  family = schemes.${familyName};
in
lib.mkIf config.services.fizzterm.enable {
  services.fizzterm.theme = {
    dark = toPalette family.dark;
    light = toPalette family.light;
  };
}
