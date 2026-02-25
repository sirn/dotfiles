{ config, lib, pkgs, ... }:

let
  # Base 16-color palette (modus-vivendi theme)
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

  # Color utility functions
  stripHash = color: builtins.substring 1 6 color;

  # Legacy color scheme for backward compatibility
  colorScheme = base16Colors;

  # Generate 256-color palette at build time using Python script
  # This uses CIELAB interpolation for perceptually uniform colors
  palette256Generator = pkgs.runCommand "generate-palette-256" {
    nativeBuildInputs = [ pkgs.python3 ];
  } ''
    cat > input.json <<'EOF'
    {
      "bg": "${base16Colors.background}",
      "fg": "${base16Colors.foreground}",
      "normal": [
        "${base16Colors.normal.black}",
        "${base16Colors.normal.red}",
        "${base16Colors.normal.green}",
        "${base16Colors.normal.yellow}",
        "${base16Colors.normal.blue}",
        "${base16Colors.normal.magenta}",
        "${base16Colors.normal.cyan}",
        "${base16Colors.normal.white}"
      ]
    }
    EOF

    python3 ${../../libexec/generate-palette.py} < input.json > $out
  '';

  # Read generated palette (IFD - Import From Derivation)
  palette240 = builtins.fromJSON (builtins.readFile palette256Generator);

  # Base 16 colors as list
  base16List = [
    base16Colors.normal.black
    base16Colors.normal.red
    base16Colors.normal.green
    base16Colors.normal.yellow
    base16Colors.normal.blue
    base16Colors.normal.magenta
    base16Colors.normal.cyan
    base16Colors.normal.white
    base16Colors.bright.black
    base16Colors.bright.red
    base16Colors.bright.green
    base16Colors.bright.yellow
    base16Colors.bright.blue
    base16Colors.bright.magenta
    base16Colors.bright.cyan
    base16Colors.bright.white
  ];

  # Full 256-color palette (0-255)
  palette256 = base16List ++ palette240;

  # Helper to get color by index
  getColor = idx: builtins.elemAt palette256 idx;

  # 256-color palette structure:
  # 0-15:   Base 16 colors (8 normal + 8 bright)
  # 16-231: 6x6x6 RGB cube (216 colors) using CIELAB interpolation
  # 232-255: 24 grayscale ramp from background to foreground
  #
  # CIELAB provides perceptually uniform color interpolation.
  #
  # Common color indices for reference:
  # 0=black, 1=red, 2=green, 3=yellow, 4=blue, 5=magenta, 6=cyan, 7=white
  # 8=bright black, 9=bright red, ..., 15=bright white
  # 16-231: Use getColor(idx) where idx = 16 + 36*r + 6*g + b, r,g,b in 0-5
  # 232-255: Grayscale from dark to light
in
{
  programs.alacritty = lib.mkIf config.programs.alacritty.enable {
    settings = {
      colors = {
        primary = {
          background = colorScheme.background;
          foreground = colorScheme.foreground;
        };

        normal = colorScheme.normal;
        bright = colorScheme.bright;
      };
    };
  };

  programs.foot = lib.mkIf config.programs.foot.enable {
    settings = {
      colors = {
        background = stripHash colorScheme.background;
        foreground = stripHash colorScheme.foreground;
      } // lib.listToAttrs (lib.imap0 (i: c: {
        name = toString i;
        value = stripHash c;
      }) palette256);
    };
  };

  programs.wezterm = lib.mkIf config.programs.wezterm.enable {
    colorSchemes = {
      modus-vivendi = {
        ansi = [
          colorScheme.normal.black
          colorScheme.normal.red
          colorScheme.normal.green
          colorScheme.normal.yellow
          colorScheme.normal.blue
          colorScheme.normal.magenta
          colorScheme.normal.cyan
          colorScheme.normal.white
        ];

        brights = [
          colorScheme.bright.black
          colorScheme.bright.red
          colorScheme.bright.green
          colorScheme.bright.yellow
          colorScheme.bright.blue
          colorScheme.bright.magenta
          colorScheme.bright.cyan
          colorScheme.bright.white
        ];

        indexed = lib.listToAttrs (lib.imap0 (i: c: {
          name = toString i;
          value = c;
        }) palette256);

        background = colorScheme.background;
        cursor_bg = colorScheme.foreground;
        cursor_border = colorScheme.foreground;
        cursor_fg = colorScheme.background;
        foreground = colorScheme.foreground;
        scrollbar_thumb = colorScheme.scrollbar;
        selection_bg = colorScheme.selection;
        selection_fg = colorScheme.background;
      };
    };
  };

  xdg.configFile."wezterm/modules/colors.lua" = lib.mkIf config.programs.wezterm.enable {
    text = ''
      return {
        color_scheme = 'modus-vivendi',
        colors = {
          tab_bar = {
            background = '${colorScheme.background}',
            new_tab = {
              bg_color = '${colorScheme.background}',
              fg_color = '${colorScheme.foreground}',
            },
            new_tab_hover = {
              bg_color = '${colorScheme.background}',
              fg_color = '${colorScheme.normal.cyan}',
            },
          },
        },
      }
    '';
  };

  xdg.configFile."wezterm/hm_colors.lua" = lib.mkIf config.programs.wezterm.enable {
    text = ''
      return {
        tab_colors = {
          active_index_bg = '${colorScheme.normal.blue}',
          active_index_fg = '${colorScheme.normal.black}',
          active_title_bg = '${colorScheme.selection}',
          active_title_fg = '${colorScheme.bright.white}',

          inactive_index_bg = '${colorScheme.bright.black}',
          inactive_index_fg = '${colorScheme.normal.white}',
          inactive_title_bg = '${colorScheme.background}',
          inactive_title_fg = '${colorScheme.normal.white}',

          status_bg = '${colorScheme.bright.black}',
          status_fg = '${colorScheme.normal.white}',
          status_icon_bg = '${colorScheme.background}',
          status_icon_fg = '${colorScheme.normal.blue}',
        },
      }
    '';
  };

  # Note: Ghostty will auto-generate the 256-color palette after version 1.2.3.
  # For now, we provide all 256 colors explicitly.
  programs.ghostty = lib.mkIf config.programs.ghostty.enable {
    settings = {
      theme = "modus-vivendi";
    };
    themes = {
      modus-vivendi = {
        background = stripHash colorScheme.background;
        cursor-color = stripHash colorScheme.foreground;
        foreground = stripHash colorScheme.foreground;
        palette = lib.imap0 (i: c: "${toString i}=${stripHash c}") palette256;
        selection-background = stripHash colorScheme.selection;
        selection-foreground = stripHash colorScheme.background;
      };
    };
  };

  wayland.windowManager.sway = lib.mkIf config.wayland.windowManager.sway.enable {
    config = {
      colors =
        let
          clear = "#ffffff00";
        in
        {
          focused = {
            background = "${colorScheme.normal.blue}99"; # 60%
            border = "${colorScheme.normal.blue}99"; # 60%
            childBorder = "${colorScheme.normal.blue}99"; # 60%
            indicator = colorScheme.normal.blue;
            text = colorScheme.foreground;
          };
          focusedInactive = {
            background = "${colorScheme.bright.black}cc"; # 80%
            border = "${colorScheme.bright.black}d8"; # 85%
            childBorder = "${colorScheme.bright.black}99"; # 60%
            indicator = colorScheme.normal.blue;
            text = colorScheme.foreground;
          };
          unfocused = {
            background = "${colorScheme.bright.black}99"; # 60%
            border = "${colorScheme.bright.black}a5"; # 65%
            childBorder = "${colorScheme.bright.black}66"; # 40%
            indicator = colorScheme.bright.black;
            text = colorScheme.foreground;
          };
          placeholder = {
            background = colorScheme.normal.yellow;
            border = colorScheme.normal.yellow;
            childBorder = colorScheme.normal.yellow;
            indicator = colorScheme.normal.yellow;
            text = colorScheme.foreground;
          };
          urgent = {
            background = colorScheme.normal.red;
            border = colorScheme.normal.red;
            childBorder = colorScheme.normal.red;
            indicator = colorScheme.normal.red;
            text = colorScheme.normal.black;
          };
        };
    };
  };

  programs.niri = lib.mkIf config.programs.niri.enable {
    settings = {
      layout = {
        focus-ring = {
          active.color = "${colorScheme.normal.blue}99"; # 60%
          inactive.color = "${colorScheme.normal.black}99"; # 60%
          urgent.color = colorScheme.normal.red;
        };
      };
    };
  };

  programs.waybar = lib.mkIf config.programs.waybar.enable {
    style = lib.mkDefault ''
      @define-color default_bg_solid ${colorScheme.background};
      @define-color default_bg alpha(@default_bg_solid, 0.6);
      @define-color default_fg ${colorScheme.foreground};
      @define-color highlight_bg ${colorScheme.normal.blue};
      @define-color highlight_fg ${colorScheme.background};
      @define-color alert_bg ${colorScheme.normal.red};
      @define-color alert_fg ${colorScheme.normal.white};
      @define-color battery_charging_bg ${getColor 195};
      @define-color battery_charging_fg ${getColor 40};
      @define-color battery_warning_bg ${getColor 208};
      @define-color battery_warning_fg ${getColor 229};
      @define-color battery_critical_bg ${getColor 1};
      @define-color battery_critical_fg ${getColor 218};

      /* -------------------------------------------------------------------------
       * Global & Bar
       */

      * {
          border-radius: 0;
          border: none;
          font-family: FontAwesome, sans-serif;
          font-size: 14px;
          min-height: 0;
          transition: background-color 0.3s ease-in-out;
      }

      window#waybar {
          background-color: @default_bg;
          color: @default_fg;
      }

      window#waybar.hidden {
          opacity: 0.3;
      }

      /* -------------------------------------------------------------------------
       * Layout
       */

      .module {
          margin: 2px 0;
      }

      .modules-left {
          padding-left: 8px;
      }

      .modules-right {
          padding-right: 8px;
      }

      .modules-right > .module {
          margin-right: 2px;
      }

      /* -------------------------------------------------------------------------
       * Module: workspace
       */

      #workspaces {
          margin: 0;
      }

      #workspaces button {
          padding: 0 8px;
          color: @default_fg;
          background-color: transparent;
          border-bottom: 2px solid transparent;
      }

      #workspaces button:hover {
          background-color: transparent;
          border-bottom: 2px solid transparent;
      }

      #workspaces button.focused {
          background-color: transparent;
          border-bottom: 2px solid @highlight_bg;
      }

      #workspaces button.urgent {
          color: @alert_fg;
          background-color: transparent;
          border-bottom: 2px solid @alert_bg;
      }

      /* -------------------------------------------------------------------------
       * Module: clock
       */

      #clock {
          font-weight: bold;
          padding: 0 10px;
      }

      /* When clock is the rightmost item, adjust the right padding so that
       * it aligns with our 12px boundary */
      .modules-right > widget:last-child > #clock {
          padding-right: 2px;
      }

      /* -------------------------------------------------------------------------
       * Module: pulseaudio
       */

      #pulseaudio {
          padding: 0 10px;
      }

      #pulseaudio.muted {
          color: ${colorScheme.bright.black};
      }

      /* -------------------------------------------------------------------------
       * Module: tray
       */

      #tray {
          background-color: alpha(@default_fg, 0.1);
          border-radius: 5px;
          padding: 0 10px;
      }

      #tray > .passive {
          -gtk-icon-effect: dim;
      }

      #tray > .needs-attention {
          -gtk-icon-effect: highlight;
          background-color: @alert_bg;
          color: @alert_fg;
      }

      /* -------------------------------------------------------------------------
       * Module: battery
       */

      #battery {
          background-color: alpha(@default_fg, 0.1);
          border-radius: 5px;
          padding: 0 10px;
      }

      #battery.charging:not(.full) {
          background-color: @battery_charging_bg;
          color: @battery_charging_fg;
      }

      #battery.warning {
          background-color: @battery_warning_bg;
          color: @battery_warning_fg;
      }

      #battery.critical {
          background-color: @battery_critical_bg;
          color: @battery_critical_fg;
      }

      /* -------------------------------------------------------------------------
       * Module: mode
       */

      #mode {
          background-color: @highlight_bg;
          border-radius: 5px;
          color: @highlight_fg;
          font-weight: bold;
          padding: 0 10px;
      }

      /* -------------------------------------------------------------------------
       * Module: idle_inhibitor
       */

      #idle_inhibitor {
          border-radius: 5px;
          padding: 0 10px;
      }

      #idle_inhibitor.activated {
          color: @highlight_fg;
          background-color: @highlight_bg;
      }

      /* -------------------------------------------------------------------------
       * Module: scratchpad
       */

      #scratchpad {
          background-color: alpha(@default_bg_solid, 0.2);
          padding: 0 10px;
      }

      /* -------------------------------------------------------------------------
       * Module: custom/media
       */

      #custom-audio_idle_inhibitor {
          background-color: @highlight_bg;
          border-radius: 5px;
          color: @highlight_fg;
          padding: 0 10px;
      }
    '';
  };
}
