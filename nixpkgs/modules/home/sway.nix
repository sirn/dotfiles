{ config, lib, pkgs, ... }:

with lib;
let
  inherit (config.lib.file) mkOutOfStoreSymlink;
  inherit (pkgs.stdenv) isLinux isDarwin;
  inherit (config.home) homeDirectory;

  dotfilesDir = "${homeDirectory}/.dotfiles";
  dotprivDir = "${homeDirectory}/.dotpriv";
in
{
  wayland.windowManager.sway = {
    enable = true;

    package =
      if config.machine.nixos.enable
      then pkgs.sway
      else null;

    config =
      let
        inherit (config.wayland.windowManager.sway.config) modifier terminal left down up right;

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
            bg = "${swayWallpaper} fill";
            scale = "1";
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
          "${modifier}+Return" = "exec ${terminal} ${pkgs.zsh}/bin/zsh";
          "${modifier}+Shift+q" = "kill";
          "${modifier}+d" = "exec ${pkgs.fuzzel}/bin/fuzzel";
          "${modifier}+Shift+c" = "reload";
          "${modifier}+Shift+e" = "exec ${swaynagBin} -t warning -m 'Really exit?' -B 'Yes, exit sway' '${swaymsgBin} exit'";

          # Focusing
          "${modifier}+${left}" = "focus left";
          "${modifier}+${down}" = "focus down";
          "${modifier}+${up}" = "focus up";
          "${modifier}+${right}" = "focus right";
          "${modifier}+Left" = "focus left";
          "${modifier}+Down" = "focus down";
          "${modifier}+Up" = "focus up";
          "${modifier}+Right" = "focus right";

          # Moving
          "${modifier}+Shift+${left}" = "move left";
          "${modifier}+Shift+${down}" = "move down";
          "${modifier}+Shift+${up}" = "move up";
          "${modifier}+Shift+${right}" = "move right";
          "${modifier}+Shift+Left" = "move left";
          "${modifier}+Shift+Down" = "move down";
          "${modifier}+Shift+Up" = "move up";
          "${modifier}+Shift+Right" = "move right";

          # Workspaces
          "${modifier}+1" = "workspace number 1";
          "${modifier}+2" = "workspace number 2";
          "${modifier}+3" = "workspace number 3";
          "${modifier}+4" = "workspace number 4";
          "${modifier}+5" = "workspace number 5";
          "${modifier}+6" = "workspace number 6";
          "${modifier}+7" = "workspace number 7";
          "${modifier}+8" = "workspace number 8";
          "${modifier}+9" = "workspace number 9";
          "${modifier}+0" = "workspace number 10";

          # Split
          "${modifier}+b" = "splith";
          "${modifier}+v" = "splitv";

          # Layouts
          "${modifier}+e" = "layout toggle split";
          "${modifier}+f" = "fullscreen";
          "${modifier}+s" = "layout stacking";
          "${modifier}+w" = "layout tabbed";
          "${modifier}+Shift+space" = "floating toggle";
          "${modifier}+Shift+grave" = "sticky toggle";

          # Focusing
          "${modifier}+space" = "focus mode_toggle";
          "${modifier}+a" = "focus parent";

          # Scratchpad
          "${modifier}+Shift+minus" = "move scratchpad";
          "${modifier}+minus" = "scratchpad show";

          # Modes
          "${modifier}+r" = "mode \"resize\"";

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
                env GRIM_DEFAULT_DIR="${homeDirectory}/Desktop" \
                ${pkgs.grim}/bin/grim -g "$(${pkgs.slurp}/bin/slurp)" && \
                ${pkgs.libnotify}/bin/notify-send \
                    "Screenshot captured" \
                    "Screenshot saved to ~/Desktop"
          '';

          # Locking
          "${modifier}+Ctrl+Shift+L" = "exec ${swaylockBin} -f -c 000000";
        };

        modes = {
          resize = {
            "${left}" = "resize shrink width 10px";
            "${down}" = "resize grow height 10px";
            "${up}" = "resize shrink height 10px";
            "${right}" = "resize grow width 10px";

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
          modifier = "${modifier}";
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
                    timeout 300 '${swaylockBin} -f -c 000000' \
                    timeout 600 '${swaymsgBin} "output * dpms off"' \
                    resume '${swaymsgBin} "output * dpms on"' \
                    before-sleep '${swaylockBin} -f -c 000000' &
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
            { command = "${pkgs.wl-clipboard}/bin/wl-paste -t text --watch ${pkgs.clipman}/bin/clipman store --no-persist"; }
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

    extraConfig = ''
      include /etc/sway/config.d/*
      include ${homeDirectory}/.config/sway/config_$(hostname)
    '';
  };

  home.file = {
    ".config/xdg-desktop-portal/portals.conf" = {
      text = ''
        [preferred]
        default = wlr
        org.freedesktop.impl.portal.AppChooser=gtk
        org.freedesktop.impl.portal.DynamicLauncher=gtk
        org.freedesktop.impl.portal.FileChooser=gtk
        org.freedesktop.impl.portal.Inhibit=gtk
        org.freedesktop.impl.portal.Notification=gtk
        org.freedesktop.impl.portal.Settings=gtk
      '';
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
