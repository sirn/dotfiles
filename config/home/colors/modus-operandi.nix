# Modus Operandi theme - light theme from Emacs
# https://github.com/protesilaos/modus-themes
{ lib, generatePalette }:

let
  base16Colors = {
    background = "#ffffff";
    foreground = "#000000";
    selection = "#bdbdbd";
    scrollbar = "#9f9f9f";

    normal = {
      black = "#000000";
      red = "#a60000";
      green = "#006800";
      yellow = "#6f5500";
      blue = "#0031a9";
      magenta = "#721045";
      cyan = "#005e8b";
      white = "#a6a6a6";
    };

    bright = {
      black = "#595959";
      red = "#972500";
      green = "#00663f";
      yellow = "#884900";
      blue = "#3548cf";
      magenta = "#531ab6";
      cyan = "#005f5f";
      white = "#ffffff";
    };
  };

  palette256 = generatePalette "modus-operandi" base16Colors;
  getColor = idx: builtins.elemAt palette256 idx;
in
{
  variant = "light";
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

    # Light theme: light bg with dark text
    battery = {
      charging.bg = getColor 46;
      charging.text = getColor 28;
      low.bg = base16Colors.normal.yellow;
      low.text = base16Colors.bright.white;
      critical.bg = base16Colors.normal.red;
      critical.text = base16Colors.bright.white;
    };
  };
}
