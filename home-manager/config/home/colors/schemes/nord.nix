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

    inactive = {
      bg = base16Colors.bright.black;
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

    urgent = {
      bg = base16Colors.normal.red;
      text = base16Colors.bright.white;
    };

    warning = {
      bg = base16Colors.normal.yellow;
      text = base16Colors.bright.white;
    };

    secondary = {
      bg = base16Colors.normal.cyan;
      text = base16Colors.bright.white;
    };

    tertiary = {
      bg = base16Colors.normal.magenta;
      text = base16Colors.bright.white;
    };

    hover = {
      bg = base16Colors.bright.black;
      text = base16Colors.foreground;
    };

    outline = base16Colors.selection;
    shadow = base16Colors.normal.black;

    # TUI text tones and surfaces.
    # Nord's bg/fg range is compressed, so these use higher ramp/cube
    # indices than modus to keep contrast legible.
    tui = {
      muted = getColor 248; # secondary text
      dim = getColor 241; # tertiary text
      surface = getColor 238; # selection / message bg
      recess = getColor 235; # tool pending bg (deeper than surface)
      success = getColor 28; # green-tinted bg
      error = getColor 94; # red-tinted bg
      label = getColor 129; # purple accent
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
