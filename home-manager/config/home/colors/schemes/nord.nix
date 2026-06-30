{ lib, generatePalette }:

let
  base16Colors = {
    background = "#2e3440";
    foreground = "#d8dee9";
    selection = "#434c5e";
    scrollbar = "#81a1c1";

    normal = {
      black = "#3b4252";
      red = "#bf616a";
      green = "#a3be8c";
      yellow = "#ebcb8b";
      blue = "#81a1c1";
      magenta = "#b48ead";
      cyan = "#88c0d0";
      white = "#e5e9f0";
    };

    bright = {
      black = "#4c566a";
      red = "#bf616a";
      green = "#a3be8c";
      yellow = "#ebcb8b";
      blue = "#81a1c1";
      magenta = "#b48ead";
      cyan = "#8fbcbb";
      white = "#eceff4";
    };
  };

  palette256 = generatePalette "nord" base16Colors;
  getColor = idx: builtins.elemAt palette256 idx;
in
{
  variant = "dark";
  inherit base16Colors palette256;

  emacsTheme = {
    packages = epkgs: [ epkgs.nord-theme ];
    customElisp = "(load-theme 'nord t)";
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
      bg = getColor 234;
      text = base16Colors.foreground;
    };

    recess = {
      bg = getColor 232;
      text = base16Colors.foreground;
    };

    # Interaction

    inactive = {
      bg = base16Colors.bright.black;
      text = base16Colors.bright.white;
    };

    hover = {
      bg = base16Colors.bright.black;
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

    muted = getColor 248;
    dim = getColor 241;
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
