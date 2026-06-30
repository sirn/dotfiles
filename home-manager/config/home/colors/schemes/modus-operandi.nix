# Modus Operandi theme - light theme from Emacs
# https://github.com/protesilaos/modus-themes
{ lib, generatePalette }:

let
  base16Colors = {
    background = "#ffffff";
    foreground = "#000000";
    selection = "#bbbbbb"; # color 237
    scrollbar = "#919191"; # color 241

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

  emacsTheme = {
    packages = epkgs: [ epkgs.modus-themes ];
    customElisp = "(load-theme 'modus-operandi t)";
  };

  semantic = {
    primary = {
      bg = base16Colors.background;
      text = base16Colors.foreground;
    };

    accent = {
      bg = base16Colors.normal.cyan;
      text = base16Colors.bright.white;
    };

    secondary = {
      bg = getColor 25;
      text = base16Colors.normal.black;
    };

    tertiary = {
      bg = base16Colors.normal.magenta;
      text = base16Colors.bright.white;
    };

    # Role

    scrollbar = {
      bg = base16Colors.scrollbar;
      text = base16Colors.bright.white;
    };

    surface = {
      bg = getColor 237;
      text = base16Colors.foreground;
    };

    recess = {
      bg = getColor 235;
      text = base16Colors.foreground;
    };

    # Interaction

    inactive = {
      bg = base16Colors.bright.black;
      text = base16Colors.bright.white;
    };

    hover = {
      bg = getColor 19;
      text = base16Colors.foreground;
    };

    focus = {
      bg = base16Colors.normal.blue;
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
      bg = getColor 22;
      text = base16Colors.foreground;
    };

    error = {
      bg = getColor 52;
      text = base16Colors.foreground;
    };

    # UI elements

    outline = base16Colors.selection;
    shadow = base16Colors.normal.black;

    # Text tone

    muted = getColor 245;
    dim = getColor 239;
    label = getColor 129;

    # Special case: battery

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
