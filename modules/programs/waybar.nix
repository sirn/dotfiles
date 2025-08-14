{ config, lib, pkgs, ... }:

let
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
        height = 30;
        spacing = 4;
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

        "clock" = {
          format = "{:%b %d, %H:%M}";
          tooltip-format = "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>";
        };

        "pulseaudio" =
          {
            format = "{icon}{format_source}";
            format-source = " ";
            format-source-muted = "";
            format-icons = {
              headphone = "";
              hands-free = "";
              headset = "";
              phone = "";
              portable = "";
              car = "";
              default = [ "" "" "" ];
            };
            on-click = "${pkgs.pavucontrol}/bin/pavucontrol";
          };
      };
    };

    style = ./waybar.css;
  };

  home.packages = with pkgs; [
    font-awesome
  ];

  wayland.windowManager.sway = {
    config = {
      bars = [ ];
    };
  };
}
