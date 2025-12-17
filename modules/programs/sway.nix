{ options, config, lib, pkgs, ... }:

let
  swayopts = options.wayland.windowManager.sway;
in
{
  wayland.windowManager.sway = {
    enable = true;
    xwayland = true;

    systemd = {
      enable = true;
    };

    # If NixGL is configured (i.e. non-NixOS), wrap with NixGL
    # so OpenGL/Vulkan libraries are available.
    #
    # We have to use options.wayland.windowManager.sway here
    # in order to get set the overrides.
    package = config.lib.nixGL.wrap swayopts.package.default;

    config =
      let
        cfg = config.wayland.windowManager.sway.config;

        swayPackage = config.wayland.windowManager.sway.package;

        swaymsgBin = "${swayPackage}/bin/swaymsg";

        swaynagBin = "${swayPackage}/bin/swaynag";
      in
      {
        modifier = "Mod4";
        left = "h";
        down = "j";
        up = "k";
        right = "l";

        gaps = {
          inner = 4; # Left + Right = 8
          smartGaps = false;
          smartBorders = "off";
        };

        fonts = {
          names = [ "monospace" ];
          size = 10.0;
        };

        output = {
          "*" = {
            scale = lib.mkDefault "1";
          };
        };

        input = {
          "*" = {
            dwt = "enabled";
          };
          "type:keyboard" = {
            xkb_options = "ctrl:nocaps";
          };
        };

        defaultWorkspace = "workspace number 1";

        keybindings = {
          "${cfg.modifier}+Return" = lib.mkDefault "exec ${cfg.terminal}";
          "${cfg.modifier}+Shift+q" = "kill";
          "${cfg.modifier}+Shift+c" = "reload";
          "${cfg.modifier}+Shift+e" = "exec ${swaynagBin} -t warning -m 'Really exit?' -B 'Yes, exit sway' '${swaymsgBin} exit'";

          # Focusing
          "${cfg.modifier}+${cfg.left}" = "focus left";
          "${cfg.modifier}+${cfg.down}" = "focus down";
          "${cfg.modifier}+${cfg.up}" = "focus up";
          "${cfg.modifier}+${cfg.right}" = "focus right";
          "${cfg.modifier}+Left" = "focus left";
          "${cfg.modifier}+Down" = "focus down";
          "${cfg.modifier}+Up" = "focus up";
          "${cfg.modifier}+Right" = "focus right";

          # Moving
          "${cfg.modifier}+Shift+${cfg.left}" = "move left";
          "${cfg.modifier}+Shift+${cfg.down}" = "move down";
          "${cfg.modifier}+Shift+${cfg.up}" = "move up";
          "${cfg.modifier}+Shift+${cfg.right}" = "move right";
          "${cfg.modifier}+Shift+Left" = "move left";
          "${cfg.modifier}+Shift+Down" = "move down";
          "${cfg.modifier}+Shift+Up" = "move up";
          "${cfg.modifier}+Shift+Right" = "move right";

          # Workspaces
          "${cfg.modifier}+1" = "workspace number 1";
          "${cfg.modifier}+2" = "workspace number 2";
          "${cfg.modifier}+3" = "workspace number 3";
          "${cfg.modifier}+4" = "workspace number 4";
          "${cfg.modifier}+5" = "workspace number 5";
          "${cfg.modifier}+6" = "workspace number 6";
          "${cfg.modifier}+7" = "workspace number 7";
          "${cfg.modifier}+8" = "workspace number 8";
          "${cfg.modifier}+9" = "workspace number 9";
          "${cfg.modifier}+0" = "workspace number 10";

          # Move workspaces
          "${cfg.modifier}+Shift+1" = "move to workspace number 1";
          "${cfg.modifier}+Shift+2" = "move to workspace number 2";
          "${cfg.modifier}+Shift+3" = "move to workspace number 3";
          "${cfg.modifier}+Shift+4" = "move to workspace number 4";
          "${cfg.modifier}+Shift+5" = "move to workspace number 5";
          "${cfg.modifier}+Shift+6" = "move to workspace number 6";
          "${cfg.modifier}+Shift+7" = "move to workspace number 7";
          "${cfg.modifier}+Shift+8" = "move to workspace number 8";
          "${cfg.modifier}+Shift+9" = "move to workspace number 9";
          "${cfg.modifier}+Shift+0" = "move to workspace number 10";

          # Split
          "${cfg.modifier}+b" = "splith";
          "${cfg.modifier}+v" = "splitv";

          # Layouts
          "${cfg.modifier}+e" = "layout toggle split";
          "${cfg.modifier}+f" = "fullscreen";
          "${cfg.modifier}+s" = "layout stacking";
          "${cfg.modifier}+w" = "layout tabbed";
          "${cfg.modifier}+Shift+space" = "floating toggle";
          "${cfg.modifier}+Shift+grave" = "sticky toggle";

          # Focusing
          "${cfg.modifier}+space" = "focus mode_toggle";
          "${cfg.modifier}+a" = "focus parent";

          # Scratchpad
          "${cfg.modifier}+Shift+minus" = "move scratchpad";
          "${cfg.modifier}+minus" = "scratchpad show";

          # Modes
          "${cfg.modifier}+r" = "mode \"resize\"";

          # Screenshots
          "Print" = ''
            exec \
                env GRIM_DEFAULT_DIR="${config.home.homeDirectory}/Desktop" \
                ${pkgs.grim}/bin/grim && \
                ${pkgs.libnotify}/bin/notify-send \
                    "Screenshot captured" \
                    "Screenshot saved to ~/Desktop"
          '';

          "Shift+Print" = ''
            exec \
                ${pkgs.grim}/bin/grim - | \
                    ${config.machine.clipboard.copy.command} && \
                ${pkgs.libnotify}/bin/notify-send \
                    "Screenshot captured" \
                    "Screenshot saved to clipboard"
          '';

          "Alt+Print" = ''
            exec \
                env GRIM_DEFAULT_DIR="${config.home.homeDirectory}/Desktop" \
                ${pkgs.grim}/bin/grim -g "$(${pkgs.slurp}/bin/slurp)" && \
                ${pkgs.libnotify}/bin/notify-send \
                    "Screenshot captured" \
                    "Screenshot saved to ~/Desktop"
          '';

          "Alt+Shift+Print" = ''
            exec \
                ${pkgs.grim}/bin/grim -g "$(${pkgs.slurp}/bin/slurp)" - | \
                    ${config.machine.clipboard.copy.command} && \
                ${pkgs.libnotify}/bin/notify-send \
                    "Screenshot captured" \
                    "Screenshot saved to clipboard"
          '';

          # Special Keys
          "XF86AudioRaiseVolume" = ''
            exec ${pkgs.wireplumber}/bin/wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.1+
          '';

          "XF86AudioLowerVolume" = ''
            exec ${pkgs.wireplumber}/bin/wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.1-
          '';

          "XF86AudioMute" = ''
            exec ${pkgs.wireplumber}/bin/wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle
          '';

          "XF86AudioMicMute" = ''
            exec ${pkgs.wireplumber}/bin/wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle
          '';

          "XF86MonBrightnessUp" = ''
            exec ${lib.getExe pkgs.brightnessctl} --class=backlight set +10%
          '';

          "XF86MonBrightnessDown" = ''
            exec ${lib.getExe pkgs.brightnessctl} --class=backlight set 10%-
          '';
        };

        modes = {
          resize = {
            "${cfg.left}" = "resize shrink width 10px";
            "${cfg.down}" = "resize grow height 10px";
            "${cfg.up}" = "resize shrink height 10px";
            "${cfg.right}" = "resize grow width 10px";

            "Left" = "resize shrink width 10px";
            "Down" = "resize grow height 10px";
            "Up" = "resize shrink height 10px";
            "Right" = "resize grow width 10px";

            "Return" = "mode \"default\"";
            "Escape" = "mode \"default\"";
          };
        };

        floating = {
          titlebar = false;
          modifier = "${cfg.modifier}";
        };

        seat = {
          "*" = {
            hide_cursor = "when-typing 5000";
          };
        };

        focus = {
          followMouse = "always";
          mouseWarping = "container";
        };

        window = {
          titlebar = false;

          border = 4; # border + gap should be 12

          commands = [
            {
              command = "inhibit_idle fullscreen";
              criteria = {
                app_id = ".*";
              };
            }
            {
              command = "inhibit_idle fullscreen";
              criteria = {
                class = ".*";
              };
            }

            {
              command = "floating enable";
              criteria = {
                app_id = "mpv";
              };
            }
            {
              command = "floating enable";
              criteria = {
                app_id = "pavucontrol";
              };
            }

            # Dolphin
            {
              command = "floating enable";
              criteria = {
                app_id = "org.kde.dolphin";
                title = "Moving.*";
              };
            }
            {
              command = "floating enable";
              criteria = {
                app_id = "org.kde.dolphin";
                title = "Copying.*";
              };
            }

            # Portals
            {
              command = "floating enable";
              criteria = {
                app_id = "org.freedesktop.impl.portal.*";
              };
            }

            # Google Chrome (annoyingly did not set app_id)
            {
              command = "floating enable";
              criteria = {
                title = "Picture in picture";
              };
            }
            {
              command = "sticky enable";
              criteria = {
                title = "Picture in picture";
              };
            }

            # Firefox
            {
              command = "floating enable";
              criteria = {
                app_id = "firefox";
                title = "Firefox - Sharing Indicator";
              };
            }
            {
              command = "floating enable";
              criteria = {
                app_id = "firefox";
                title = "Picture-in-Picture";
              };
            }
            {
              command = "sticky enable";
              criteria = {
                app_id = "firefox";
                title = "Picture-in-Picture";
              };
            }

            # Looking Glass
            {
              command = "fullscreen";
              criteria = {
                app_id = "looking-glass-client";
              };
            }
          ];
        };
      };
  };
}
