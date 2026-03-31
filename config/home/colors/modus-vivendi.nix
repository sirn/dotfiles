# Modus Vivendi theme - dark theme from Emacs
{ lib, generatePalette }:

let
  base16Colors = {
    background = "#000000";
    foreground = "#ffffff";
    selection = "#535353";
    scrollbar = "#646464";

    normal = {
      black = "#000000";
      red = "#ff5f59";
      green = "#44bc44";
      yellow = "#d0bc00";
      blue = "#2fafff";
      magenta = "#feacd0";
      cyan = "#00d3d0";
      white = "#d0d0d0";
    };

    bright = {
      black = "#383838";
      red = "#ff5f5f";
      green = "#44df44";
      yellow = "#efef00";
      blue = "#338fff";
      magenta = "#ff66ff";
      cyan = "#00eff0";
      white = "#ffffff";
    };
  };

  palette256 = generatePalette "modus-vivendi" base16Colors;
  getColor = idx: builtins.elemAt palette256 idx;
in
{
  variant = "dark";
  inherit base16Colors palette256;

  semantic = {
    background = base16Colors.background;
    selection = base16Colors.selection;
    scrollbar = base16Colors.scrollbar;

    focus = base16Colors.normal.blue;
    hover = base16Colors.normal.cyan;
    urgent = base16Colors.normal.red;
    warning = base16Colors.normal.yellow;
    success = base16Colors.normal.green;
    vcs = base16Colors.normal.magenta;

    primary = {
      text = base16Colors.foreground;
    };

    accent = {
      text = base16Colors.bright.white;
    };

    inactive = {
      bg = base16Colors.bright.black;
      text = base16Colors.bright.white;
    };

    battery = {
      charging.bg = getColor 46;
      charging.text = getColor 22;
      low.bg = base16Colors.bright.yellow;
      low.text = base16Colors.bright.black;
      critical.bg = base16Colors.bright.red;
      critical.text = base16Colors.bright.black;
    };
  };
}
