# Nord themes - snow-storm (light) and polar-night (dark)
# https://www.nordtheme.com
# Semantic Material roles aligned to Noctalia's built-in Nord palette
# (https://github.com/noctalia-dev/noctalia src/theme/builtin_palettes.cpp).
{ lib, generatePalette }:

let
  # Nord palette — https://www.nordtheme.com
  nord0 = "#2e3440"; # Polar Night
  nord1 = "#3b4252";
  nord2 = "#434c5e";
  nord3 = "#4c566a";
  nord4 = "#d8dee9"; # Snow Storm
  nord5 = "#e5e9f0";
  nord6 = "#eceff4";
  nord7 = "#8fbcbb"; # Frost
  nord8 = "#88c0d0";
  nord9 = "#81a1c1";
  nord10 = "#5e81ac";
  nord11 = "#bf616a"; # Aurora
  nord12 = "#d08770";
  nord13 = "#ebcb8b";
  nord14 = "#a3be8c";
  nord15 = "#b48ead";

  nordLightFg = "#414858";
  nordLightSelectionFg = "#4c556a";
  nordLightCyan = "#7bb3c3";
  nordLightGreen = "#96b17f";
  nordLightYellow = "#c5a565";
  nordLightWhite = "#a5abb6";
  nordLightBrightCyan = "#82afae";
  nordLightOutline = "#c5cedd";
  nordDarkBrightBlack = "#596377";
  nordDarkCursorText = "#282828";
  nordDarkOutline = "#505a70";

  lightBase16 = {
    background = nord6; # bg
    foreground = nordLightFg; # 7.95 (AAA)
    selection = nord4; # 1.17 (FAIL)
    selectionFg = nordLightSelectionFg; # 5.52 (AA)
    cursor = nordLightCyan; # 2.01 (FAIL)
    cursorText = nord1; # 4.35 (3:1)
    scrollbar = nord10; # 3.50 (3:1)

    normal = {
      black = nord1; # 8.73 (AAA)
      red = nord11; # 3.55 (3:1)
      green = nordLightGreen; # 2.05 (FAIL)
      yellow = nordLightYellow; # 2.04 (FAIL)
      blue = nord9; # 2.34 (FAIL)
      magenta = nord15; # 2.46 (FAIL)
      cyan = nordLightCyan; # 2.01 (FAIL)
      white = nordLightWhite; # 2.00 (FAIL)
    };

    bright = {
      black = nord3; # 6.40 (AA)
      red = nord11; # 3.55 (3:1)
      green = nordLightGreen; # 2.05 (FAIL)
      yellow = nordLightYellow; # 2.04 (FAIL)
      blue = nord9; # 2.34 (FAIL)
      magenta = nord15; # 2.46 (FAIL)
      cyan = nordLightBrightCyan; # 2.10 (FAIL)
      white = nord6; # 1.00 (FAIL)
    };
  };
  lightPalette = generatePalette "nord-snow-storm" lightBase16;

  darkBase16 = {
    background = nord0; # bg
    foreground = nord4; # 9.25 (AAA)
    selection = nord6; # 10.84 (AAA)
    selectionFg = nord3; # 6.40 (AA)
    cursor = nord6; # 10.84 (AAA)
    cursorText = nordDarkCursorText; # 12.79 (AAA)
    scrollbar = nord9; # 4.64 (AA)

    normal = {
      black = nord1; # 1.24 (FAIL)
      red = nord11; # 3.05 (3:1)
      green = nord14; # 6.13 (AA)
      yellow = nord13; # 8.00 (AAA)
      blue = nord9; # 4.64 (AA)
      magenta = nord15; # 4.41 (3:1)
      cyan = nord8; # 6.24 (AA)
      white = nord5; # 10.26 (AAA)
    };

    bright = {
      black = nordDarkBrightBlack; # 2.07 (FAIL)
      red = nord11; # 3.05 (3:1)
      green = nord14; # 6.13 (AA)
      yellow = nord13; # 8.00 (AAA)
      blue = nord9; # 4.64 (AA)
      magenta = nord15; # 4.41 (3:1)
      cyan = nord7; # 5.99 (AA)
      white = nord6; # 10.84 (AAA)
    };
  };
  darkPalette = generatePalette "nord-polar-night" darkBase16;
in
{
  light =
    let
      base16Colors = lightBase16;
      getColor = idx: builtins.elemAt lightPalette idx;
    in
    {
      name = "nord-snow-storm";
      base16Colors = lightBase16;
      palette256 = lightPalette;
      emacsTheme = {
        packages = epkgs: [ epkgs.nord-theme ];
        customElisp = "(load-theme 'tango t)";
      };
      semantic = {
        primary = {
          bg = base16Colors.background; # surface
          text = nord0; # onSurface
        }; # 10.84 (AAA)

        accent = {
          bg = base16Colors.normal.cyan;
          text = base16Colors.foreground;
        }; # 3.96 (3:1)

        secondary = {
          bg = "#64adc2"; # Noctalia secondary
          text = base16Colors.bright.white; # onSecondary
        }; # 2.20 (FAIL)

        tertiary = {
          bg = "#6fa9a8"; # Noctalia tertiary
          text = base16Colors.bright.white; # onTertiary
        }; # 2.30 (FAIL)

        # Role

        scrollbar = {
          bg = base16Colors.scrollbar;
          text = base16Colors.bright.white;
        }; # 3.50 (3:1)

        surface = {
          bg = getColor 254;
          text = base16Colors.foreground;
        }; # 6.93 (AA)

        recess = {
          bg = getColor 250;
          text = base16Colors.foreground;
        }; # 5.24 (AA)

        # Interaction

        inactive = {
          bg = nord5; # surfaceVariant
          text = base16Colors.bright.black; # onSurfaceVariant
        }; # 6.06 (AA)

        hover = {
          bg = "#6fa9a8"; # Noctalia hover
          text = base16Colors.bright.white; # onHover
        }; # 2.30 (FAIL)

        focus = {
          bg = nord10; # Noctalia primary
          text = base16Colors.bright.white; # onPrimary
        }; # 3.50 (3:1)

        urgent = {
          bg = base16Colors.normal.red; # error
          text = base16Colors.bright.white; # onError
        }; # 3.55 (3:1)

        warning = {
          bg = base16Colors.normal.yellow;
          text = base16Colors.foreground;
        }; # 3.90 (3:1)

        success = {
          bg = getColor 194;
          text = getColor 22;
        }; # 4.71 (AA)

        error = {
          bg = getColor 225;
          text = getColor 88;
        }; # 4.90 (AA)

        # UI elements

        outline = nordLightOutline; # 1.38 (FAIL)
        shadow = nord4; # 1.17 (FAIL)

        # Text tone

        muted = nord3; # 6.40 (AA)
        dim = getColor 18; # 4.78 (AA)
        label = getColor 219; # 1.40 (FAIL)

        # Special case: battery

        battery = {
          charging = {
            bg = getColor 46;
            text = base16Colors.foreground;
          }; # 3.88 (3:1)
          low = {
            bg = base16Colors.normal.yellow;
            text = base16Colors.foreground;
          }; # 3.90 (3:1)
          critical = {
            bg = base16Colors.normal.red;
            text = base16Colors.bright.white;
          }; # 3.55 (3:1)
        };
      };
    };

  dark =
    let
      base16Colors = darkBase16;
      getColor = idx: builtins.elemAt darkPalette idx;
    in
    {
      name = "nord-polar-night";
      base16Colors = darkBase16;
      palette256 = darkPalette;
      emacsTheme = {
        packages = epkgs: [ epkgs.nord-theme ];
        customElisp = "(load-theme 'nord t)";
      };
      semantic = {
        primary = {
          bg = base16Colors.background; # surface
          text = base16Colors.bright.white; # onSurface
        }; # 10.84 (AAA)

        accent = {
          bg = base16Colors.normal.cyan;
          text = base16Colors.bright.black;
        }; # 3.02 (3:1)

        secondary = {
          bg = base16Colors.normal.cyan; # Noctalia secondary (Nord8)
          text = base16Colors.background; # onSecondary
        }; # 6.24 (AA)

        tertiary = {
          bg = nord10; # Noctalia tertiary
          text = base16Colors.background; # onTertiary
        }; # 3.10 (3:1)

        # Role

        scrollbar = {
          bg = base16Colors.scrollbar;
          text = base16Colors.bright.white;
        }; # 2.34 (FAIL)

        surface = {
          bg = getColor 234;
          text = base16Colors.foreground;
        }; # 7.02 (AAA)

        recess = {
          bg = getColor 232;
          text = base16Colors.foreground;
        }; # 8.45 (AAA)

        # Interaction

        inactive = {
          bg = base16Colors.normal.black; # surfaceVariant (Nord1)
          text = base16Colors.foreground; # onSurfaceVariant
        }; # 7.45 (AAA)

        hover = {
          bg = nord10; # Noctalia hover
          text = base16Colors.background; # onHover
        }; # 3.10 (3:1)

        focus = {
          bg = base16Colors.bright.cyan; # Noctalia primary (Nord7)
          text = base16Colors.background; # onPrimary
        }; # 5.99 (AA)

        urgent = {
          bg = base16Colors.normal.red; # error
          text = base16Colors.background; # onError
        }; # 3.05 (3:1)

        warning = {
          bg = base16Colors.normal.yellow;
          text = base16Colors.bright.white;
        }; # 1.35 (FAIL)

        success = {
          bg = getColor 22;
          text = base16Colors.foreground;
        }; # 6.42 (AA)

        error = {
          bg = getColor 52;
          text = base16Colors.foreground;
        }; # 7.44 (AAA)

        # UI elements

        outline = nordDarkOutline; # 1.81 (FAIL)
        shadow = base16Colors.background; # 1.00 (FAIL)

        # Text tone

        muted = getColor 146; # 5.94 (AA)
        dim = getColor 247; # 4.53 (AA)
        label = getColor 213; # 6.06 (AA)

        # Special case: battery

        battery = {
          charging = {
            bg = getColor 46;
            text = getColor 28;
          }; # 2.87 (FAIL)
          low = {
            bg = base16Colors.normal.yellow;
            text = base16Colors.bright.white;
          }; # 1.35 (FAIL)
          critical = {
            bg = base16Colors.normal.red;
            text = base16Colors.bright.white;
          }; # 3.55 (3:1)
        };
      };
    };
}
