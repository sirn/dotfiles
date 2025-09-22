{ config, lib, pkgs, ... }:

let
  cfg = config.programs.waybar;

  swaycfg = config.wayland.windowManager.sway;

  niricfg = config.programs.niri;
in
{
  programs.waybar = {
    enable = true;

    systemd = {
      enable = true;
    };

    settings = {
      mainBar = {
        height = 32;
        spacing = 2;
        layer = "top";

        modules-left = (if swaycfg.enable then [
          "sway/workspaces"
          "sway/mode"
          "sway/scratchpad"
        ] else [ ]) ++ (if niricfg.enable then [
          "niri/workspaces"
        ] else [ ]) ++ [
          "custom/media"
        ];

        modules-center = (if swaycfg.enable then [
          "sway/window"
        ] else [ ]) ++ (if niricfg.enable then [
          "niri/window"
        ] else [ ]);

        modules-right = [
          "idle_inhibitor"
          "pulseaudio"
          "tray"
          "battery"
          "clock"
        ];

        "sway/mode" = lib.mkIf swaycfg.enable {
          format = "<span style = \"italic\">{}</span>";
        };

        "sway/scratchpad" = lib.mkIf swaycfg.enable {
          format = "{icon} {count}";
          show-empty = false;
          format-icons = [ "" "" ];
          tooltip = true;
          tooltip-format = "{app}: {title}";
        };

        "idle_inhibitor" = {
          format = "{icon}";
          format-icons = {
            activated = "";
            deactivated = "";
          };
        };

        "tray" = {
          spacing = 10;
        };

        "battery" = {
          interval = 60;
          states = {
            full = 100;
            normal = 80;
            warning = 30;
            critical = 15;
          };
          format = "{capacity}% {icon}";
          format-icons = [ "" "" "" "" "" ];
        };

        "clock" = {
          format = "{:%b %d, %H:%M}";
          tooltip-format = "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>";
        };

        "pulseaudio" =
          {
            format = "{icon}";
            format-bluetooth = "";
            format-muted = "";
            format-source = "";
            format-icons = {
              headphone = "";
              hands-free = "";
              headset = "";
              phone = "";
              portable = "";
              car = "";
              default = [ "" "" ];
            };
            on-click = "${pkgs.pavucontrol}/bin/pavucontrol";
          };
      };
    };
  };

  home.packages = with pkgs; [
    font-awesome
  ];

  wayland.windowManager.sway = lib.mkIf swaycfg.enable {
    config = {
      bars = [ ];
      keybindings = lib.mkOptionDefault {
        "${swaycfg.config.modifier}+Shift+b" = "exec pkill -SIGUSR1 waybar";
      };
    };
  };

  programs.niri.settings = lib.mkIf niricfg.enable {
    binds = {
      "Mod+Shift+B".action.spawn = [ "sh" "-c" "pkill -SIGUSR1 waybar" ];
    };
  };
}
