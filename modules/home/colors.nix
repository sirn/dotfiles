{ config, lib, ... }:

let
  colorScheme = {
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

  stripHash = color: builtins.substring 1 6 color;
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
        regular0 = stripHash colorScheme.normal.black;
        regular1 = stripHash colorScheme.normal.red;
        regular2 = stripHash colorScheme.normal.green;
        regular3 = stripHash colorScheme.normal.yellow;
        regular4 = stripHash colorScheme.normal.blue;
        regular5 = stripHash colorScheme.normal.magenta;
        regular6 = stripHash colorScheme.normal.cyan;
        regular7 = stripHash colorScheme.normal.white;
        bright0 = stripHash colorScheme.bright.black;
        bright1 = stripHash colorScheme.bright.red;
        bright2 = stripHash colorScheme.bright.green;
        bright3 = stripHash colorScheme.bright.yellow;
        bright4 = stripHash colorScheme.bright.blue;
        bright5 = stripHash colorScheme.bright.magenta;
        bright6 = stripHash colorScheme.bright.cyan;
        bright7 = stripHash colorScheme.bright.white;
      };
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
          active_bg = '${colorScheme.normal.cyan}',
          active_fg = '${colorScheme.normal.black}',
          border_bg = '${colorScheme.background}',
          inactive_bg = '${colorScheme.bright.black}',
          inactive_fg = '${colorScheme.normal.white}',
          remote_bg = '${colorScheme.normal.blue}',
          remote_fg = '${colorScheme.normal.black}',
        },
      }
    '';
  };

  programs.ghostty = lib.mkIf config.programs.ghostty.enable {
    settings = {
      theme = "modus-vivendi";
    };
    themes = {
      modus-vivendi = {
        background = stripHash colorScheme.background;
        cursor-color = stripHash colorScheme.foreground;
        foreground = stripHash colorScheme.foreground;
        palette = [
          "0=${stripHash colorScheme.normal.black}"
          "1=${stripHash colorScheme.normal.red}"
          "2=${stripHash colorScheme.normal.green}"
          "3=${stripHash colorScheme.normal.yellow}"
          "4=${stripHash colorScheme.normal.blue}"
          "5=${stripHash colorScheme.normal.magenta}"
          "6=${stripHash colorScheme.normal.cyan}"
          "7=${stripHash colorScheme.normal.white}"
          "8=${stripHash colorScheme.bright.black}"
          "9=${stripHash colorScheme.bright.red}"
          "10=${stripHash colorScheme.bright.green}"
          "11=${stripHash colorScheme.bright.yellow}"
          "12=${stripHash colorScheme.bright.blue}"
          "13=${stripHash colorScheme.bright.magenta}"
          "14=${stripHash colorScheme.bright.cyan}"
          "15=${stripHash colorScheme.bright.white}"
        ];
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
      @define-color default_bg rgba(0, 0, 0, 0.6);
      @define-color default_fg ${colorScheme.foreground};
      @define-color highlight_bg ${colorScheme.normal.blue};
      @define-color highlight_fg ${colorScheme.background};
      @define-color alert_bg ${colorScheme.normal.red};
      @define-color alert_fg ${colorScheme.normal.white};

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
          background-color: rgba(255, 255, 255, 0.1);
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
          background-color: rgba(255, 255, 255, 0.1);
          border-radius: 5px;
          padding: 0 10px;
      }

      #battery.charging:not(.full) {
          background-color: #e8f5e9;
          color: #2e7d32;
      }

      #battery.warning {
          background-color: #ff6f00;
          color: #ffecb3;
      }

      #battery.critical {
          background-color: #b71c1c;
          color: #ffcdd2;
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
          background-color: rgba(0, 0, 0, 0.2);
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
