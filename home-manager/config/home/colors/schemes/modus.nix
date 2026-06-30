# Modus themes (Emacs) - operandi (light) and vivendi (dark)
# https://github.com/protesilaos/modus-themes
# Semantic Material roles aligned to the modus-themes palette
# (bg-dim, fg-dim, bg-hover, border, accent foregrounds).
{ lib, generatePalette }:

let
  # Modus Operandi (light)
  lightBase16 = {
    background = "#ffffff"; # bg
    foreground = "#000000"; # 21.00 (AAA)
    selection = "#bbbbbb"; # 1.92 (FAIL)
    scrollbar = "#919191"; # 3.15 (3:1)

    normal = {
      black = "#000000"; # 21.00 (AAA)
      red = "#a60000"; # 8.01 (AAA)
      green = "#006800"; # 7.05 (AAA)
      yellow = "#6f5500"; # 7.06 (AAA)
      blue = "#0031a9"; # 10.44 (AAA)
      magenta = "#721045"; # 11.20 (AAA)
      cyan = "#005e8b"; # 7.06 (AAA)
      white = "#a6a6a6"; # 2.43 (FAIL)
    };

    bright = {
      black = "#595959"; # 7.00 (AAA)
      red = "#972500"; # 8.14 (AAA)
      green = "#00663f"; # 7.07 (AAA)
      yellow = "#884900"; # 7.00 (AAA)
      blue = "#3548cf"; # 7.05 (AAA)
      magenta = "#531ab6"; # 9.58 (AAA)
      cyan = "#005f5f"; # 7.49 (AAA)
      white = "#ffffff"; # 1.00 (FAIL)
    };
  };
  lightPalette = generatePalette "modus-operandi" lightBase16;

  # Modus Vivendi (dark)
  darkBase16 = {
    background = "#000000"; # bg
    foreground = "#ffffff"; # 21.00 (AAA)
    selection = "#393939"; # 1.82 (FAIL)
    scrollbar = "#686868"; # 3.77 (3:1)

    normal = {
      black = "#000000"; # 1.00 (FAIL)
      red = "#ff5f59"; # 7.03 (AAA)
      green = "#44bc44"; # 8.52 (AAA)
      yellow = "#d0bc00"; # 10.88 (AAA)
      blue = "#2fafff"; # 8.70 (AAA)
      magenta = "#feacd0"; # 12.03 (AAA)
      cyan = "#00d3d0"; # 11.23 (AAA)
      white = "#d0d0d0"; # 13.62 (AAA)
    };

    bright = {
      black = "#383838"; # 1.79 (FAIL)
      red = "#ff5f5f"; # 7.05 (AAA)
      green = "#44df44"; # 11.88 (AAA)
      yellow = "#efef00"; # 17.02 (AAA)
      blue = "#338fff"; # 6.51 (AA)
      magenta = "#ff66ff"; # 8.60 (AAA)
      cyan = "#00eff0"; # 14.60 (AAA)
      white = "#ffffff"; # 21.00 (AAA)
    };
  };
  darkPalette = generatePalette "modus-vivendi" darkBase16;
in
{
  light =
    let
      base16Colors = lightBase16;
      getColor = idx: builtins.elemAt lightPalette idx;
    in
    {
      name = "modus-operandi";
      base16Colors = lightBase16;
      palette256 = lightPalette;
      emacsTheme = {
        packages = epkgs: [ epkgs.modus-themes ];
        customElisp = "(load-theme 'modus-operandi t)";
      };
      semantic = {
        primary = {
          bg = base16Colors.background; # surface
          text = base16Colors.foreground; # onSurface
        }; # 21.00 (AAA)

        accent = {
          bg = base16Colors.normal.cyan;
          text = base16Colors.bright.white;
        }; # 7.06 (AAA)

        secondary = {
          bg = base16Colors.normal.cyan; # modus cyan
          text = base16Colors.bright.white; # onSecondary
        }; # 7.06 (AAA)

        tertiary = {
          bg = base16Colors.normal.magenta; # modus magenta
          text = base16Colors.bright.white; # onTertiary
        }; # 11.20 (AAA)

        # Role

        scrollbar = {
          bg = base16Colors.scrollbar;
          text = base16Colors.bright.white;
        }; # 3.15 (3:1)

        surface = {
          bg = "#f2f2f2"; # modus bg-dim
          text = base16Colors.foreground;
        }; # 18.76 (AAA)

        recess = {
          bg = "#f0f0f0"; # modus bg-alt
          text = base16Colors.foreground;
        }; # 18.43 (AAA)

        # Interaction

        inactive = {
          bg = "#f2f2f2"; # modus bg-dim (surfaceVariant)
          text = base16Colors.bright.black; # modus fg-dim (onSurfaceVariant)
        }; # 6.26 (AA)

        hover = {
          bg = "#b2e4dc"; # modus bg-hover
          text = base16Colors.foreground; # onHover
        }; # 15.02 (AAA)

        focus = {
          bg = base16Colors.normal.blue; # modus blue (primary)
          text = base16Colors.bright.white; # onPrimary
        }; # 10.44 (AAA)

        urgent = {
          bg = base16Colors.normal.red; # modus red (error)
          text = base16Colors.bright.white; # onError
        }; # 8.01 (AAA)

        warning = {
          bg = base16Colors.normal.yellow;
          text = base16Colors.bright.white;
        }; # 7.06 (AAA)

        success = {
          bg = getColor 22;
          text = base16Colors.foreground;
        }; # 15.24 (AAA)

        error = {
          bg = getColor 52;
          text = base16Colors.foreground;
        }; # 14.91 (AAA)

        # UI elements

        outline = getColor 241; # modus border, 3.15 (3:1)
        shadow = base16Colors.normal.black; # 21.00 (AAA)

        # Text tone

        muted = getColor 245; # 5.57 (AA)
        dim = getColor 244; # 4.81 (AA)
        label = getColor 129; # 10.89 (AAA)

        # Special case: battery

        battery = {
          charging = {
            bg = getColor 46;
            text = getColor 28;
          }; # 3.57 (3:1)
          low = {
            bg = base16Colors.normal.yellow;
            text = base16Colors.bright.white;
          }; # 7.06 (AAA)
          critical = {
            bg = base16Colors.normal.red;
            text = base16Colors.bright.white;
          }; # 8.01 (AAA)
        };
      };
    };

  dark =
    let
      base16Colors = darkBase16;
      getColor = idx: builtins.elemAt darkPalette idx;
    in
    {
      name = "modus-vivendi";
      base16Colors = darkBase16;
      palette256 = darkPalette;
      emacsTheme = {
        packages = epkgs: [ epkgs.modus-themes ];
        customElisp = "(load-theme 'modus-vivendi t)";
      };
      semantic = {
        primary = {
          bg = base16Colors.background; # surface
          text = base16Colors.foreground; # onSurface
        }; # 21.00 (AAA)

        accent = {
          bg = base16Colors.normal.cyan;
          text = base16Colors.bright.black;
        }; # 6.27 (AA)

        secondary = {
          bg = base16Colors.normal.cyan; # modus cyan
          text = base16Colors.background; # onSecondary (black on bright cyan)
        }; # 11.23 (AAA)

        tertiary = {
          bg = base16Colors.normal.magenta; # modus magenta (bright pink)
          text = base16Colors.background; # onTertiary (black on light pink)
        }; # 12.03 (AAA)

        # Role

        scrollbar = {
          bg = base16Colors.scrollbar;
          text = base16Colors.bright.white;
        }; # 5.57 (AA)

        surface = {
          bg = "#1e1e1e"; # modus bg-dim
          text = base16Colors.foreground;
        }; # 16.67 (AAA)

        recess = {
          bg = "#161616"; # modus bg-alt
          text = base16Colors.foreground;
        }; # 18.10 (AAA)

        # Interaction

        inactive = {
          bg = "#1e1e1e"; # modus bg-dim (surfaceVariant)
          text = "#989898"; # modus fg-dim (onSurfaceVariant)
        }; # 5.78 (AA)

        hover = {
          bg = "#45605e"; # modus bg-hover
          text = base16Colors.bright.white; # onHover (white on dark teal)
        }; # 6.80 (AA)

        focus = {
          bg = base16Colors.normal.blue; # modus blue (primary)
          text = base16Colors.background; # onPrimary (black on bright blue)
        }; # 8.70 (AAA)

        urgent = {
          bg = base16Colors.normal.red; # modus red (error)
          text = base16Colors.background; # onError (black on bright red)
        }; # 7.03 (AAA)

        warning = {
          bg = base16Colors.normal.yellow;
          text = base16Colors.bright.black;
        }; # 6.07 (AA)

        success = {
          bg = getColor 22;
          text = base16Colors.foreground;
        }; # 15.71 (AAA)

        error = {
          bg = getColor 52;
          text = base16Colors.foreground;
        }; # 16.32 (AAA)

        # UI elements

        outline = "#646464"; # modus border, 3.55 (3:1)
        shadow = base16Colors.normal.black; # 1.00 (FAIL)

        # Text tone

        muted = getColor 245; # 5.77 (AA)
        dim = getColor 244; # 5.03 (AA)
        label = getColor 129; # 10.64 (AAA)

        # Special case: battery

        battery = {
          charging = {
            bg = getColor 46;
            text = getColor 22;
          }; # 6.38 (AA)
          low = {
            bg = base16Colors.bright.yellow;
            text = base16Colors.bright.black;
          }; # 9.50 (AAA)
          critical = {
            bg = base16Colors.bright.red;
            text = base16Colors.bright.black;
          }; # 3.94 (3:1)
        };
      };
    };
}
