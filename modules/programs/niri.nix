{ lib, config, pkgs, ... }:

let
  cfg = config.programs.niri;
in
{
  programs.niri = {
    enable = true;

    package = config.lib.nixGL.wrap pkgs.niri;

    settings = {
      screenshot-path = "${config.home.homeDirectory}/Desktop/Screenshot from %Y-%m-%d %H-%M-%S.png";

      prefer-no-csd = true;

      layout = {
        gaps = 8;
        always-center-single-column = true;
        focus-ring = {
          width = 4; # focus-ring + gap should be 12
        };
      };

      cursor = {
        hide-when-typing = true;
        theme = config.gtk.cursorTheme.name;
        size = config.gtk.cursorTheme.size;
      };

      input = {
        focus-follows-mouse = {
          enable = true;
        };

        keyboard = {
          xkb = {
            options = "ctrl:nocaps";
          };
        };
      };

      binds = {
        # Basic keybindings
        "Mod+Shift+Slash".action.show-hotkey-overlay = { };

        # System volume controls
        "XF86AudioRaiseVolume".action.spawn = [ "${pkgs.wireplumber}/bin/wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "0.1+" ];
        "XF86AudioLowerVolume".action.spawn = [ "${pkgs.wireplumber}/bin/wpctl" "set-volume" "@DEFAULT_AUDIO_SINK@" "0.1-" ];
        "XF86AudioMute".action.spawn = [ "${pkgs.wireplumber}/bin/wpctl" "set-mute" "@DEFAULT_AUDIO_SINK@" "toggle" ];
        "XF86AudioMicMute".action.spawn = [ "${pkgs.wireplumber}/bin/wpctl" "set-mute" "@DEFAULT_AUDIO_SOURCE@" "toggle" ];

        # Brightness controls
        "XF86MonBrightnessUp".action.spawn = [ "${lib.getExe pkgs.brightnessctl}" "--class=backlight" "set" "+10%" ];
        "XF86MonBrightnessDown".action.spawn = [ "${lib.getExe pkgs.brightnessctl}" "--class=backlight" "set" "10%-" ];

        # Overview
        "Mod+O".action.toggle-overview = { };

        # Window management
        "Mod+Q".action.close-window = { };

        # Focus navigation
        "Mod+Left".action.focus-column-left = { };
        "Mod+Down".action.focus-window-down = { };
        "Mod+Up".action.focus-window-up = { };
        "Mod+Right".action.focus-column-right = { };
        "Mod+H".action.focus-column-left = { };
        "Mod+J".action.focus-window-down = { };
        "Mod+K".action.focus-window-up = { };
        "Mod+L".action.focus-column-right = { };

        # Move windows/columns
        "Mod+Ctrl+Left".action.move-column-left = { };
        "Mod+Ctrl+Down".action.move-window-down = { };
        "Mod+Ctrl+Up".action.move-window-up = { };
        "Mod+Ctrl+Right".action.move-column-right = { };
        "Mod+Ctrl+H".action.move-column-left = { };
        "Mod+Ctrl+J".action.move-window-down = { };
        "Mod+Ctrl+K".action.move-window-up = { };
        "Mod+Ctrl+L".action.move-column-right = { };

        # Focus/move to first/last column
        "Mod+Home".action.focus-column-first = { };
        "Mod+End".action.focus-column-last = { };
        "Mod+Ctrl+Home".action.move-column-to-first = { };
        "Mod+Ctrl+End".action.move-column-to-last = { };

        # Monitor focus
        "Mod+Shift+Left".action.focus-monitor-left = { };
        "Mod+Shift+Down".action.focus-monitor-down = { };
        "Mod+Shift+Up".action.focus-monitor-up = { };
        "Mod+Shift+Right".action.focus-monitor-right = { };
        "Mod+Shift+H".action.focus-monitor-left = { };
        "Mod+Shift+J".action.focus-monitor-down = { };
        "Mod+Shift+K".action.focus-monitor-up = { };
        "Mod+Shift+L".action.focus-monitor-right = { };

        # Move column to monitor
        "Mod+Shift+Ctrl+Left".action.move-column-to-monitor-left = { };
        "Mod+Shift+Ctrl+Down".action.move-column-to-monitor-down = { };
        "Mod+Shift+Ctrl+Up".action.move-column-to-monitor-up = { };
        "Mod+Shift+Ctrl+Right".action.move-column-to-monitor-right = { };
        "Mod+Shift+Ctrl+H".action.move-column-to-monitor-left = { };
        "Mod+Shift+Ctrl+J".action.move-column-to-monitor-down = { };
        "Mod+Shift+Ctrl+K".action.move-column-to-monitor-up = { };
        "Mod+Shift+Ctrl+L".action.move-column-to-monitor-right = { };

        # Workspace navigation
        "Mod+Page_Down".action.focus-workspace-down = { };
        "Mod+Page_Up".action.focus-workspace-up = { };
        "Mod+U".action.focus-workspace-down = { };
        "Mod+I".action.focus-workspace-up = { };
        "Mod+Ctrl+Page_Down".action.move-column-to-workspace-down = { };
        "Mod+Ctrl+Page_Up".action.move-column-to-workspace-up = { };
        "Mod+Ctrl+U".action.move-column-to-workspace-down = { };
        "Mod+Ctrl+I".action.move-column-to-workspace-up = { };

        # Move workspace
        "Mod+Shift+Page_Down".action.move-workspace-down = { };
        "Mod+Shift+Page_Up".action.move-workspace-up = { };
        "Mod+Shift+U".action.move-workspace-down = { };
        "Mod+Shift+I".action.move-workspace-up = { };

        # Workspace switching by number
        "Mod+1".action.focus-workspace = 1;
        "Mod+2".action.focus-workspace = 2;
        "Mod+3".action.focus-workspace = 3;
        "Mod+4".action.focus-workspace = 4;
        "Mod+5".action.focus-workspace = 5;
        "Mod+6".action.focus-workspace = 6;
        "Mod+7".action.focus-workspace = 7;
        "Mod+8".action.focus-workspace = 8;
        "Mod+9".action.focus-workspace = 9;
        "Mod+Ctrl+1".action.move-column-to-workspace = 1;
        "Mod+Ctrl+2".action.move-column-to-workspace = 2;
        "Mod+Ctrl+3".action.move-column-to-workspace = 3;
        "Mod+Ctrl+4".action.move-column-to-workspace = 4;
        "Mod+Ctrl+5".action.move-column-to-workspace = 5;
        "Mod+Ctrl+6".action.move-column-to-workspace = 6;
        "Mod+Ctrl+7".action.move-column-to-workspace = 7;
        "Mod+Ctrl+8".action.move-column-to-workspace = 8;
        "Mod+Ctrl+9".action.move-column-to-workspace = 9;

        # Column management
        "Mod+BracketLeft".action.consume-or-expel-window-left = { };
        "Mod+BracketRight".action.consume-or-expel-window-right = { };
        "Mod+Comma".action.consume-window-into-column = { };
        "Mod+Period".action.expel-window-from-column = { };

        # Window sizing and layout
        "Mod+R".action.switch-preset-column-width = { };
        "Mod+Shift+R".action.switch-preset-window-height = { };
        "Mod+Ctrl+R".action.reset-window-height = { };
        "Mod+F".action.maximize-column = { };
        "Mod+Shift+F".action.fullscreen-window = { };
        "Mod+Ctrl+F".action.expand-column-to-available-width = { };
        "Mod+C".action.center-column = { };
        "Mod+Ctrl+C".action.center-visible-columns = { };

        # Width adjustments
        "Mod+Minus".action.set-column-width = "-10%";
        "Mod+Equal".action.set-column-width = "+10%";

        # Height adjustments
        "Mod+Shift+Minus".action.set-window-height = "-10%";
        "Mod+Shift+Equal".action.set-window-height = "+10%";

        # Floating window toggle
        "Mod+V".action.toggle-window-floating = { };
        "Mod+Shift+V".action.switch-focus-between-floating-and-tiling = { };

        # Column tabbed display
        "Mod+W".action.toggle-column-tabbed-display = { };

        # Screenshots
        "Print".action.screenshot = { };
        "Ctrl+Print".action.screenshot-screen = { };
        "Alt+Print".action.screenshot-window = { };

        # System control
        "Mod+Escape".action.toggle-keyboard-shortcuts-inhibit = { };
        "Mod+Shift+E".action.quit = { };
        "Ctrl+Alt+Delete".action.quit = { };
        "Mod+Shift+P".action.power-off-monitors = { };
      };

      switch-events = lib.mkIf config.machine.isLaptop {
        "lid-open".action.spawn = [ "${lib.getExe cfg.package}" "msg" "output" "eDP-1" "on" ];
        "lid-close".action.spawn = [ "${lib.getExe cfg.package}" "msg" "output" "eDP-1" "off" ];
      };

      window-rules = [
        {
          clip-to-geometry = true;
          geometry-corner-radius = {
            bottom-left = 4.0;
            bottom-right = 4.0;
            top-left = 4.0;
            top-right = 4.0;
          };
        }
        {
          matches = [{ app-id = "mpv"; }];
          open-floating = true;
        }
        {
          matches = [{ app-id = "pavucontrol"; }];
          open-floating = true;
        }

        # Dolphin
        {
          matches = [{ app-id = "org.kde.dolphin"; title = "Moving.*"; }];
          open-floating = true;
        }
        {
          matches = [{ app-id = "org.kde.dolphin"; title = "Copying.*"; }];
          open-floating = true;
        }

        # Portals
        {
          matches = [{ app-id = "org.freedesktop.impl.portal.*"; }];
          open-floating = true;
        }

        # Google Chrome (annoyingly did not set app_id)
        {
          matches = [{ app-id = "firefox"; title = "Picture in picture"; }];
          open-floating = true;
        }

        # Firefox
        {
          matches = [{ app-id = "firefox"; title = "Picture-in-Picture"; }];
          open-floating = true;
        }
        {
          matches = [{ app-id = "firefox"; title = "Firefox - Sharing Indicator"; }];
          open-floating = true;
        }

        # Looking Glass
        {
          matches = [{ app-id = "looking-glass-client"; }];
          open-fullscreen = true;
        }
      ];
    };
  };

  # Nix don't install services if the package is installed through Home Manager due to
  # lib/systemd/user vs config/systemd/user difference. This needs to be created manually
  # based on ${cfg.package}/lib/systemd/user/niri.service.
  #
  # Note: intentionally hard-coding graphical-session.target for consistency with
  # graphical-session-pre.target.
  systemd.user.services.niri = lib.mkIf cfg.enable {
    Unit = {
      Description = "A scrollable-tiling Wayland compositor";
      BindsTo = [ "graphical-session.target" ];

      Before =
        [ "graphical-session.target" ] ++
        lib.optional config.xdg.autostart.enable [ "xdg-desktop-autostart.target" ];

      After = [ "graphical-session-pre.target" ];
      Wants =
        [ "graphical-session-pre.target" ] ++
        lib.optional config.xdg.autostart.enable [ "xdg-desktop-autostart.target" ];

      # Avoid killing Niri session.
      X-RestartIfChanged = false;
    };

    Service = {
      Slice = "session.slice";
      Type = "notify";
      ExecStart = "${cfg.package}/bin/niri --session";
    };
  };

  systemd.user.targets.niri-shutdown = lib.mkIf cfg.enable {
    Unit = {
      Description = "Shutdown running niri session";
      DefaultDependencies = false;
      StopWhenUnneeded = true;
      Conflicts = [ "graphical-session.target" "graphical-session-pre.target" ];
      After = [ "graphical-session.target" "graphical-session-pre.target" ];
    };
  };
}
