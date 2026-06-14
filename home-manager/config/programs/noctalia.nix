{ config, lib, ... }:

let
  noctaliaCfg = config.programs.noctalia;

  swaycfg = config.wayland.windowManager.sway;

  niricfg = config.programs.niri;

  noctaliaShell = lib.getExe noctaliaCfg.package;

  radiusSize = 12.0;
in
{
  programs.noctalia = {
    enable = true;
    settings = {
      # Shell
      shell = {
        font_family = "sans-serif";
        settings_show_advanced = true;
        panel = {
          launcher_placement = "centered";
          control_center_placement = "attached";
          open_near_click_control_center = true;
        };
      };

      # Bar
      bar = {
        position = "left";
        widgets = {
          position = "left";
          thickness = 40;
          widget_spacing = 10;
          radius_top_left = 0;
          radius_top_right = radiusSize;
          radius_bottom_left = 0;
          radius_bottom_right = radiusSize;
          margin_edge = 0;
          margin_ends = 8; # Consistent with Niri/Sway gaps
          start = [ "launcher" ];
          center = [ "workspaces" ];
          end = [
            "tray"
            "notifications"
            "battery"
            "clock"
            "control-center"
            "session"
          ];
        };
      };

      # Per-widget settings
      widget = {
        launcher = {
          use_distro_logo = true;
          enable_colorization = true;
        };
        tray = {
          drawer = true;
        };
        battery = {
          show_label = false;
          warning_threshold = 30;
        };
        workspaces = {
          display = "none";
        };
        clock = {
          format = "{:%H:%M}";
          vertical_format = "{:%H\n%M}";
        };
      };

      # Control Center
      control_center = {
        shortcuts = [
          { type = "wifi"; }
          { type = "bluetooth"; }
          { type = "notification"; }
          { type = "power_profile"; }
          { type = "caffeine"; }
          { type = "nightlight"; }
        ];
      };

      # Desktop widgets
      desktop_widgets = {
        enabled = false;
      };

      # Dock
      dock = {
        enabled = false;
      };

      # Location
      location = {
        address = "Tokyo, Japan";
        sunrise = "06:30";
        sunset = "18:30";
      };

      # Idle
      idle = {
        behavior_order = lib.mkDefault [
          "lock"
          "screen-off"
        ];
        behavior = {
          lock = {
            action = "lock";
            enabled = true;
            timeout = 300;
          };
          "screen-off" = {
            action = "screen_off";
            enabled = true;
            timeout = 600;
          };
        };
      };

      # Night Light
      nightlight = {
        enabled = true;
        temperature_day = 6500;
        temperature_night = 4500;
      };

      # Theme
      theme = {
        templates = {
          enable_builtin_templates = false;
          enable_community_templates = false;
        };
      };

      # Wallpaper
      wallpaper = {
        enabled = true;
        directory = "${config.home.homeDirectory}/Pictures/Wallpapers";
        fill_mode = "crop";
        automation = {
          enabled = true;
          interval_minutes = 30;
          order = "random";
          recursive = true;
        };
      };

      # Brightness
      brightness = {
        enable_ddcutil = true;
      };

      # Lockscreen widgets
      lockscreen_widgets = {
        enabled = false;
      };
    };
  };

  wayland.windowManager.sway = lib.mkIf (noctaliaCfg.enable && swaycfg.enable) {
    config = {
      startup = lib.mkAfter [ { command = noctaliaShell; } ];

      bars = [ ];

      keybindings = {
        "${swaycfg.config.modifier}+d" = "exec ${noctaliaShell} msg panel-toggle launcher";
      };
    };
  };

  programs.niri = lib.mkIf (noctaliaCfg.enable && niricfg.enable) {
    settings = {
      spawn-at-startup = lib.mkAfter [ { argv = [ noctaliaShell ]; } ];

      window-rules = lib.mkBefore [
        {
          clip-to-geometry = true;
          geometry-corner-radius = {
            bottom-left = radiusSize;
            bottom-right = radiusSize;
            top-left = radiusSize;
            top-right = radiusSize;
          };
        }
      ];

      binds = {
        "Mod+d".action.spawn = [
          "${noctaliaShell}"
          "msg"
          "panel-toggle"
          "launcher"
        ];
      };
    };
  };
}
