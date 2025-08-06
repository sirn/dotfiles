{ config, lib, pkgs, ... }:

{
  programs.waybar = {
    enable = true;

    systemd = {
      enable = true;

      target = "sway-session.target";
    };

    settings = {
      mainBar = {
        height = 30;
        spacing = 4;
        layer = "top";

        modules-left = [ "sway/workspaces" "sway/mode" "sway/scratchpad" "custom/media" ];
        modules-center = [ "sway/window" ];
        modules-right = [ "idle_inhibitor" "pulseaudio" "tray" "clock" ];

        "sway/mode" = {
          format = "<span style = \"italic\">{}</span>";
        };

        "sway/scratchpad" = {
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
