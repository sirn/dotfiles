{ config, lib, pkgs, ... }:

let
  inherit (lib) elemAt mkDefault;
  inherit (builtins) match;
  inherit (config.home) homeDirectory;
in
{
  imports = [
    ../programs/foot.nix
    ../programs/fuzzel.nix
    ../programs/waybar.nix
  ];

  wayland.windowManager.sway = {
    enable = true;
    xwayland = true;
    systemdIntegration = config.machine.nixos.enable;

    package =
      if config.machine.nixos.enable
      then pkgs.sway
      else null;

    config =
      let
        cfg = config.wayland.windowManager.sway.config;

        swaymsgBin =
          if config.machine.nixos.enable
          then "${pkgs.sway-unwrapped}/bin/swaymsg"
          else "swaymsg";

        swaynagBin =
          if config.machine.nixos.enable
          then "${pkgs.sway-unwrapped}/bin/swaynag"
          else "swaynag";

        swayWallpaper =
          if config.machine.nixos.enable
          then "${pkgs.sway-unwrapped}/share/backgrounds/sway/Sway_Wallpaper_Blue_1920x1080.png"
          else "/usr/share/backgrounds/sway/Sway_Wallpaper_Blue_1920x1080.png";

        # swaylock needs to access PAM, so we must use the system package on non-NixOS
        swaylockBin =
          if config.machine.nixos.enable
          then "${pkgs.swaylock}/bin/swaylock"
          else "swaylock";

        bgSplit = match "^(.+)[[:space:]]+(stretch|fill|fit|center|tile).*" cfg.output."*".bg;
        bg = elemAt bgSplit 0;
        bgMode = elemAt bgSplit 1;
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

        terminal = "${pkgs.foot}/bin/foot";

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
          "${cfg.modifier}+Return" = "exec ${cfg.terminal} ${pkgs.zsh}/bin/zsh";
          "${cfg.modifier}+Shift+q" = "kill";
          "${cfg.modifier}+d" = "exec ${pkgs.fuzzel}/bin/fuzzel";
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

          # Locking
          "${cfg.modifier}+Ctrl+Shift+L" = "exec ${swaylockBin} -f -i ${bg} -s ${bgMode}";
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

        bars = [
          {
            command = "${pkgs.waybar}/bin/waybar";
          }
        ];

        startup =
          let
            schema = "org.gnome.desktop.interface";
          in
          [
            {
              command = ''
                ${pkgs.swayidle}/bin/swayidle -w \
                    timeout 300 '${swaylockBin} -f -i ${bg} -s ${bgMode}' \
                    timeout 600 '${swaymsgBin} "output * dpms off"' \
                    resume '${swaymsgBin} "output * dpms on"' \
                    before-sleep '${swaylockBin} -f -i ${bg} -s ${bgMode}' &
              '';
            }

            { command = "gsettings set ${schema} document-font-name \"Noto Sans 10\""; always = true; }
            { command = "gsettings set ${schema} font-name \"Noto Sans 10\""; always = true; }
            { command = "gsettings set ${schema} icon-theme \"Breeze\""; always = true; }
            { command = "gsettings set ${schema} gtk-theme \"Breeze\""; always = true; }
            { command = "gsettings set ${schema} cursor-theme \"breeze_cursors\""; always = true; }
            { command = "gsettings set ${schema} cursor-size 24"; always = true; }
            { command = "gsettings set ${schema} monospace-font-name \"Hack 10\""; always = true; }

            { command = "pipewire"; }
            { command = "${pkgs.wl-clipboard}/bin/wl-paste -pw ${pkgs.wl-clipboard}/wl-copy"; }
            { command = "fcitx5 -r"; }
            { command = "${pkgs.mako}/bin/mako"; }
            { command = "${homeDirectory}/.local/libexec/start-xdg-portals"; }
          ];

        seat = {
          "*" = {
            xcursor_theme = "breeze_cursors 24";
            hide_cursor = "when-typing enable";
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
          ];
        };
      };
  };

  home.file = {
    ".config/xdg-desktop-portal/portals.conf" = {
      text = lib.generators.toINI { } {
        preferred = {
          default = "wlr";
          "org.freedesktop.impl.portal.AppChooser" = "gtk";
          "org.freedesktop.impl.portal.DynamicLauncher" = "gtk";
          "org.freedesktop.impl.portal.FileChooser" = "gtk";
          "org.freedesktop.impl.portal.Inhibit" = "gtk";
          "org.freedesktop.impl.portal.Notification" = "gtk";
          "org.freedesktop.impl.portal.Settings" = "gtk";
        };
      };
    };

    ".local/libexec/start-xdg-portals" = {
      executable = true;
      text = ''
        #!${pkgs.bash}/bin/bash
        pkill -f xdg-desktop-portal

        run_and_disown() {
            "$@" &
            sleep 0.5
            disown
        }

        run_and_disown /usr/libexec/xdg-desktop-portal-wlr
        run_and_disown /usr/libexec/xdg-desktop-portal-gtk
        run_and_disown /usr/libexec/xdg-desktop-portal -vr
      '';
    };
  };

  home.sessionVariablesExtra = ''
    export XDG_CURRENT_DESKTOP=sway
    export QT_QPA_PLATFORMTHEME=kde
  '';
}
