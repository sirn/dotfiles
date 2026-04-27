{ config, lib, ... }:

let
  cfg = config.programs.noctalia-shell;

  swaycfg = config.wayland.windowManager.sway;

  niricfg = config.programs.niri;

  noctaliaShell = lib.getExe cfg.package;
in
{
  programs.noctalia-shell = {
    enable = true;
    settings = {
      appLauncher = {
        position = "center";
      };
      bar = {
        density = "default";
        position = "left";
        showCapsule = false;
        widgets = {
          left = [
            {
              id = "Launcher";
              useDistroLogo = true;
              enableColorization = true;
            }
          ];
          center = [
            {
              hideUnoccupied = false;
              id = "Workspace";
              labelMode = "none";
            }
          ];
          right = [
            { id = "Tray"; }
            {
              alwaysShowPercentage = false;
              id = "Battery";
              warningThreshold = 30;
            }
            {
              id = "ControlCenter";
            }
            {
              formatHorizontal = "HH:mm";
              formatVertical = "HH mm";
              id = "Clock";
              useMonospacedFont = true;
              usePrimaryColor = true;
            }
          ];
        };
      };
      colorSchemes = {
        syncGsettings = false;
      };
      controlCenter = {
        position = "close_to_bar_button";
        diskPath = "/";
        shortcuts = {
          left = [
            { id = "Network"; }
            { id = "Bluetooth"; }
            { id = "WallpaperSelector"; }
            { id = "NoctaliaPerformance"; }
          ];
          right = [
            { id = "Notifications"; }
            { id = "PowerProfile"; }
            { id = "KeepAwake"; }
            { id = "NightLight"; }
          ];
        };
        cards = [
          {
            enabled = true;
            id = "profile-card";
          }
          {
            enabled = true;
            id = "shortcuts-card";
          }
          {
            enabled = true;
            id = "audio-card";
          }
          {
            enabled = false;
            id = "brightness-card";
          }
          {
            enabled = true;
            id = "weather-card";
          }
          {
            enabled = true;
            id = "media-sysmon-card";
          }
        ];
      };
      dock = {
        enabled = false;
      };
      location = {
        monthBeforeDay = true;
        name = "Tokyo, Japan";
      };
      idle = {
        enabled = true;
        lockTimeout = "120";
        screenOffTimeout = "180";
        suspendTimeout = lib.mkDefault "0";
      };
      nightLight = {
        autoSchedule = true;
        dayTemp = "6500";
        enabled = true;
        manualSunrise = "06:30";
        manualSunset = "08:30";
        nightTemp = "4500";
      };
      wallpaper = {
        automationEnabled = true;
        directory = "${config.home.homeDirectory}/Pictures/Wallpapers";
        randomIntervalSec = 1800;
        wallpaperChangeMode = "random";
      };
    };
  };

  wayland.windowManager.sway = lib.mkIf (cfg.enable && swaycfg.enable) {
    config = {
      startup = lib.mkAfter [ { command = noctaliaShell; } ];

      bars = [ ];

      keybindings = {
        "${swaycfg.config.modifier}+d" = "exec ${noctaliaShell} ipc call launcher toggle";
      };
    };
  };

  programs.niri = lib.mkIf (cfg.enable && niricfg.enable) {
    settings = {
      spawn-at-startup = lib.mkAfter [ { argv = [ noctaliaShell ]; } ];

      window-rules = lib.mkBefore [
        {
          clip-to-geometry = true;
          geometry-corner-radius = {
            bottom-left = 15.0;
            bottom-right = 15.0;
            top-left = 15.0;
            top-right = 15.0;
          };
        }
      ];

      binds = {
        "Mod+d".action.spawn = [
          "${noctaliaShell}"
          "ipc"
          "call"
          "launcher"
          "toggle"
        ];
      };
    };
  };
}
