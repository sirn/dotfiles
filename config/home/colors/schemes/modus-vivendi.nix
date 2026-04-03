# Modus Vivendi theme - dark theme from Emacs
# https://github.com/protesilaos/modus-themes
{ lib, generatePalette }:

let
  base16Colors = {
    background = "#000000";
    foreground = "#ffffff";
    selection = "#393939"; # color 237
    scrollbar = "#686868"; # color 241

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
      text = base16Colors.bright.black;
    };

    urgent = {
      bg = base16Colors.normal.red;
      text = base16Colors.bright.black;
    };

    warning = {
      bg = base16Colors.normal.yellow;
      text = base16Colors.bright.black;
    };

    battery = {
      charging = {
        bg = getColor 46;
        text = getColor 22;
      };
      low = {
        bg = base16Colors.bright.yellow;
        text = base16Colors.bright.black;
      };
      critical = {
        bg = base16Colors.bright.red;
        text = base16Colors.bright.black;
      };
    };
  };
}
