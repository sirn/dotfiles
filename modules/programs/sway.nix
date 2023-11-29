{ config, lib, pkgs, ... }:

let
  inherit (lib) mkDefault mkIf;
  inherit (config.home) homeDirectory;
in
mkIf config.desktop.enable {
  wayland.windowManager.sway = {
    enable = true;
    xwayland = true;

    systemd = {
      enable = config.machine.isNixOS;
    };

    package =
      if config.machine.isNixOS
      then pkgs.sway
      else null;

    config =
      let
        cfg = config.wayland.windowManager.sway.config;
        swayPackage = config.wayland.windowManager.sway.package;

        swaymsgBin =
          if swayPackage != null
          then "${swayPackage}/bin/swaymsg"
          else "swaymsg";

        swaynagBin =
          if swayPackage != null
          then "${swayPackage}/bin/swaynag"
          else "swaynag";

        swayWallpaper =
          if swayPackage != null
          then "${swayPackage}/share/backgrounds/sway/Sway_Wallpaper_Blue_1920x1080.png"
          else "/usr/share/backgrounds/sway/Sway_Wallpaper_Blue_1920x1080.png";
      in
      {
        modifier = "Mod4";
        left = "h";
        down = "j";
        up = "k";
        right = "l";

        fonts = {
          names = [ "monospace" ];
          size = 10.0;
        };

        output = {
          "*" = {
            bg = mkDefault "${swayWallpaper} fill";
            scale = mkDefault "1";
          };
        };

        input = {
          "*" = {
            dwt = "enabled";
            natural_scroll = "enabled";
          };
        };

        defaultWorkspace = "workspace number 1";

        keybindings = {
          "${cfg.modifier}+Return" = "exec ${cfg.terminal}";
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
                env GRIM_DEFAULT_DIR="${homeDirectory}/Desktop" \
                ${pkgs.grim}/bin/grim && \
                ${pkgs.libnotify}/bin/notify-send \
                    "Screenshot captured" \
                    "Screenshot saved to ~/Desktop"
          '';

          "Shift+Print" = ''
            exec \
                ${pkgs.grim}/bin/grim - | \
                    ${pkgs.wl-clipboard}/bin/wl-copy -t image/png && \
                ${pkgs.libnotify}/bin/notify-send \
                    "Screenshot captured" \
                    "Screenshot saved to clipboard"
          '';

          "Alt+Print" = ''
            exec \
                env GRIM_DEFAULT_DIR="${homeDirectory}/Desktop" \
                ${pkgs.grim}/bin/grim -g "$(${pkgs.slurp}/bin/slurp)" && \
                ${pkgs.libnotify}/bin/notify-send \
                    "Screenshot captured" \
                    "Screenshot saved to ~/Desktop"
          '';

          "Alt+Shift+Print" = ''
            exec \
                ${pkgs.grim}/bin/grim -g "$(${pkgs.slurp}/bin/slurp)" - | \
                    ${pkgs.wl-clipboard}/bin/wl-copy -t image/png && \
                ${pkgs.libnotify}/bin/notify-send \
                    "Screenshot captured" \
                    "Screenshot saved to clipboard"
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
          titlebar = true;
          modifier = "${cfg.modifier}";
        };

        seat = {
          "*" = {
            hide_cursor = "when-typing disable";
          };
        };

        window = {
          titlebar = true;
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

            # Portals
            {
              command = "floating enable";
              criteria = {
                app_id = "org.freedesktop.impl.portal.*";
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
              command = "floating enable";
              criteria = {
                app_id = "firefox";
                title = "Extension:.*1Password.*";
              };
            }
            {
              command = "sticky enable";
              criteria = {
                app_id = "firefox";
                title = "Picture-in-Picture";
              };
            }
          ];
        };
      };
  };

  home.sessionVariablesExtra = ''
    export XDG_CURRENT_DESKTOP=sway

    # https://github.com/swaywm/sway/issues/3814
    # https://github.com/swaywm/wlroots/issues/3189
    export WLR_NO_HARDWARE_CURSORS=1
  '';
}
