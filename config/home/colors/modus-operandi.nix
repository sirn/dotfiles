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
    primary = {
      bg = base16Colors.background;
      text = base16Colors.foreground;
    };

    inactive = {
      bg = base16Colors.bright.black;
      text = base16Colors.bright.white;
    };

    selection = {
      bg = base16Colors.selection;
      text = base16Colors.bright.white;
    };

    scrollbar = {
      bg = base16Colors.scrollbar;
      text = base16Colors.bright.white;
    };

    focus = {
      bg = base16Colors.normal.blue;
      text = base16Colors.bright.white;
    };

    hover = {
      bg = base16Colors.normal.cyan;
      text = base16Colors.bright.white;
    };

    urgent = {
      bg = base16Colors.normal.red;
      text = base16Colors.bright.white;
    };

    warning = {
      bg = base16Colors.normal.yellow;
      text = base16Colors.bright.white;
    };

    success = {
      bg = base16Colors.normal.green;
      text = base16Colors.bright.white;
    };

    important = {
      bg = base16Colors.normal.magenta;
      text = base16Colors.bright.white;
    };

    battery = {
      charging = {
        bg = getColor 46;
        text = getColor 28;
      };
      low = {
        bg = base16Colors.normal.yellow;
        text = base16Colors.bright.white;
      };
      critical = {
        bg = base16Colors.normal.red;
        text = base16Colors.bright.white;
      };
    };
  };
}
