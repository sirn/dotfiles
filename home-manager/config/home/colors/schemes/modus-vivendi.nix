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

  emacsTheme = {
    packages = epkgs: [ epkgs.modus-themes ];
    customElisp = "(load-theme 'modus-vivendi t)";
  };

  semantic = {
    primary = {
      bg = base16Colors.background;
      text = base16Colors.foreground;
    };

    accent = {
      bg = base16Colors.normal.cyan;
      text = base16Colors.bright.black;
    };

    secondary = {
      bg = getColor 25;
      text = base16Colors.bright.white;
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
      text = base16Colors.bright.white;
    };

    focus = {
      bg = base16Colors.normal.blue;
      text = base16Colors.bright.white;
    };

    urgent = {
      bg = base16Colors.normal.red;
      text = base16Colors.bright.black;
    };

    warning = {
      bg = base16Colors.normal.yellow;
      text = base16Colors.bright.black;
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
