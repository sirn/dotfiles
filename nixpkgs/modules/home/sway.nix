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
  home.file = {
    ".config/foot/foot.ini" = {
      text = ''
        bold-text-in-bright=yes
        dpi-aware=no
        font=PragmataPro Mono:size=12
      '';
    };

    ".config/fuzzel/fuzzel.ini" = {
      text = ''
        font=monospace:size=12
        dpi-aware=no
        terminal=foot -e
        width=40
        line-height=18
        horizontal-pad=8
        vertical-pad=4
        layer=overlay

        [colors]
        background=1e2225fa
        selection=285577ff
        border=494e52ff
        text=999999ff
        match=ffffffff
        selection-text=ddddddff
        selection-match=ffffffff

        [border]
        radius=0
      '';
    };

    ".config/sway/config" = {
      text = ''
        ### Variables
        #
        set $mod   Mod4
        set $left  h
        set $down  j
        set $up    k
        set $right l
        set $term  foot zsh
        set $menu  fuzzel


        ### Output configuration
        # See also: man 5 sway-output
        #
        output * bg    /usr/share/backgrounds/sway/Sway_Wallpaper_Blue_1920x1080.png fill
        output * scale 1


        ### Input configuration
        # See also: man 5 sway-input
        #
        input "*" {
            dwt enabled
            natural_scroll enabled
        }


        ### Key bindings
        #

        # Basic:
        #
        bindsym $mod+Return  exec $term
        bindsym $mod+Shift+q kill
        bindsym $mod+d       exec $menu
        bindsym $mod+Shift+c reload
        bindsym $mod+Shift+e exec swaynag -t warning -m 'Really exit?' -B 'Yes, exit sway' 'swaymsg exit'

        floating_modifier $mod normal

        # Moving around:
        #
        bindsym $mod+$left  focus left
        bindsym $mod+$down  focus down
        bindsym $mod+$up    focus up
        bindsym $mod+$right focus right

        bindsym $mod+Left   focus left
        bindsym $mod+Down   focus down
        bindsym $mod+Up     focus up
        bindsym $mod+Right  focus right

        bindsym $mod+Shift+$left  move left
        bindsym $mod+Shift+$down  move down
        bindsym $mod+Shift+$up    move up
        bindsym $mod+Shift+$right move right

        bindsym $mod+Shift+Left   move left
        bindsym $mod+Shift+Down   move down
        bindsym $mod+Shift+Up     move up
        bindsym $mod+Shift+Right  move right

        # Switch to workspace:
        #
        bindsym $mod+1 workspace number 1
        bindsym $mod+2 workspace number 2
        bindsym $mod+3 workspace number 3
        bindsym $mod+4 workspace number 4
        bindsym $mod+5 workspace number 5
        bindsym $mod+6 workspace number 6
        bindsym $mod+7 workspace number 7
        bindsym $mod+8 workspace number 8
        bindsym $mod+9 workspace number 9
        bindsym $mod+0 workspace number 10

        # Move focused container to workspace:
        #
        bindsym $mod+Shift+1 move container to workspace number 1
        bindsym $mod+Shift+2 move container to workspace number 2
        bindsym $mod+Shift+3 move container to workspace number 3
        bindsym $mod+Shift+4 move container to workspace number 4
        bindsym $mod+Shift+5 move container to workspace number 5
        bindsym $mod+Shift+6 move container to workspace number 6
        bindsym $mod+Shift+7 move container to workspace number 7
        bindsym $mod+Shift+8 move container to workspace number 8
        bindsym $mod+Shift+9 move container to workspace number 9
        bindsym $mod+Shift+0 move container to workspace number 10

        # Split current object:
        #
        bindsym $mod+b splith
        bindsym $mod+v splitv

        # Different layout styles:
        #
        bindsym $mod+e           layout toggle split
        bindsym $mod+f           fullscreen
        bindsym $mod+s           layout stacking
        bindsym $mod+w           layout tabbed

        # Floating & sticky:
        #
        bindsym $mod+Shift+space floating toggle
        bindsym $mod+Shift+grave sticky toggle

        # Focusing:
        #
        bindsym $mod+space focus mode_toggle
        bindsym $mod+a     focus parent

        # Scratchpad:
        #
        bindsym $mod+Shift+minus move scratchpad
        bindsym $mod+minus       scratchpad show

        # Resizing:
        #
        mode "resize" {
            bindsym $left  resize shrink width  10px
            bindsym $down  resize grow   height 10px
            bindsym $up    resize shrink height 10px
            bindsym $right resize grow   width  10px

            bindsym Left   resize shrink width  10px
            bindsym Down   resize grow   height 10px
            bindsym Up     resize shrink height 10px
            bindsym Right  resize grow   width  10px

            bindsym Return mode "default"
            bindsym Escape mode "default"
        }

        bindsym $mod+r mode "resize"

        # Screenshot:
        #
        bindsym Print exec \
            env GRIM_DEFAULT_DIR="${homeDirectory}/Desktop" \
            grim && notify-send \
                "Screenshot captured" \
                "Screenshot saved to ~/Desktop"

        bindsym Shift+Print exec \
            env GRIM_DEFAULT_DIR="${homeDirectory}/Desktop" \
            grim -g "$(slurp)" && notify-send \
                "Screenshot captured" \
                "Screenshot saved to ~/Desktop"


        ###
        # Status Bar
        # See also: man 5 sway-bar
        #
        bar {
            swaybar_command waybar
        }


        # Locking
        #
        bindsym $mod+Ctrl+Shift+L exec swaylock -f -c 000000


        ###
        # Idling
        #
        exec sway-audio-idle-inhibit
        exec swayidle -w \
            timeout 300 'swaylock -f -c 000000' \
            timeout 600 'swaymsg "output * dpms off"' \
            resume 'swaymsg "output * dpms on"' \
            before-sleep 'swaylock -f -c 000000' &


        ###
        # Themes
        #
        seat * xcursor_theme breeze_cursors 24
        set $gnome-schema org.gnome.desktop.interface

        exec_always {
            gsettings set $gnome-schema document-font-name  "Noto Sans 10"
            gsettings set $gnome-schema font-name           "Noto Sans 10"
            gsettings set $gnome-schema icon-theme          "Breeze"
            gsettings set $gnome-schema gtk-theme           "Breeze"
            gsettings set $gnome-schema cursor-theme        "breeze_cursors"
            gsettings set $gnome-schema cursor-size         24
            gsettings set $gnome-schema monospace-font-name "Hack 10"
        }


        ###
        # External programs
        #
        exec pipewire
        exec wl-paste -t text --watch clipman store --no-persist
        exec fcitx5 -r
        exec mako

        # Portals:
        #
        exec /usr/libexec/xdg-desktop-portal-wlr
        exec /usr/libexec/xdg-desktop-portal-gtk
        exec /usr/libexec/xdg-desktop-portal -r


        ###
        # Window properties
        #

        # Inhibit sleep if any apps is running in full screen
        for_window [app_id=".*"] inhibit_idle fullscreen
        for_window [class=".*"]  inhibit_idle fullscreen

        for_window [app_id="firefox" title="Firefox — Sharing Indicator"] floating enable
        for_window [app_id="firefox" title="Picture-in-Picture"]          floating enable
        for_window [app_id="firefox" title="Picture-in-Picture"]          sticky enable

        for_window [app_id="mpv"]         floating enable
        for_window [app_id="pavucontrol"] floating enable


        ### Include
        #
        include /etc/sway/config.d/*
        include ${homeDirectory}/.config/sway/config_$(hostname)
      '';
    };

    ".config/waybar/config" = {
      text = ''
        {
            // ------------------------------------------------------------------------
            // Global configurations
            // ------------------------------------------------------------------------
            "height": 30,
            "spacing": 4,

            // ------------------------------------------------------------------------
            // Modules
            // ------------------------------------------------------------------------
            "modules-left": [
                "sway/workspaces",
                "sway/mode",
                "sway/scratchpad",
                "custom/media"
            ],

            "modules-center": [
                "sway/window"
            ],

            "modules-right": [
                "idle_inhibitor",
                "pulseaudio",
                "tray",
                "clock"
            ],

            // ------------------------------------------------------------------------
            // Module configurations
            // ------------------------------------------------------------------------
            "sway/mode": {
                "format": "<span style=\"italic\">{}</span>"
            },

            "sway/scratchpad": {
                "format": "{icon} {count}",
                "show-empty": false,
                "format-icons": ["", ""],
                "tooltip": true,
                "tooltip-format": "{app}: {title}"
            },

            "idle_inhibitor": {
                "format": "{icon}",
                "format-icons": {
                    "activated": "",
                    "deactivated": ""
                }
            },

            "tray": {
                "spacing": 10
            },

            "clock": {
                "format": "{:%b %d, %H:%M}",
                "tooltip-format": "<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>"
            },

            "pulseaudio": {
                "format": "{icon}{format_source}",
                "format-source": " ",
                "format-source-muted": "",
                "format-icons": {
                    "headphone": "",
                    "hands-free": "",
                    "headset": "",
                    "phone": "",
                    "portable": "",
                    "car": "",
                    "default": ["", "", ""]
                },
                "on-click": "pavucontrol"
            }
        }
      '';
    };

    ".config/waybar/style.css" = {
      text = ''
        * {
            font-family: FontAwesome, Roboto, Helvetica, Arial, sans-serif;
            font-size: 13px;
        }

        window#waybar {
            background-color: rgba(30, 34, 37, 0.5);
            color: #ffffff;
            transition-property: background-color;
            transition-duration: .5s;
        }

        window#waybar.hidden {
            opacity: 0.2;
        }

        button {
            box-shadow: inset 0 -3px transparent;
            border: none;
            border-radius: 0;
        }

        /* https://github.com/Alexays/Waybar/wiki/FAQ#the-workspace-buttons-have-a-strange-hover-effect */
        button:hover {
            background: inherit;
            box-shadow: inset 0 -3px #ffffff;
        }

        #workspaces button {
            padding: 0 5px;
            background-color: transparent;
            color: #ffffff;
        }

        #workspaces button:hover {
            background: rgba(0, 0, 0, 0.2);
        }

        #workspaces button.focused {
            background-color: #64727D;
            box-shadow: inset 0 -3px #ffffff;
        }

        #workspaces button.urgent {
            background-color: #eb4d4b;
        }

        #clock,
        #tray,
        #mode,
        #pulseaudio,
        #idle_inhibitor,
        #scratchpad {
            padding: 0 10px;
            background-color: rgba(100, 114, 125, 0.3);
            color: #ffffff;
        }

        #window,
        #workspaces {
            margin: 0 4px;
        }

        /* If workspaces is the leftmost module, omit left margin */
        .modules-left > widget:first-child > #workspaces {
            margin-left: 0;
        }

        /* If workspaces is the rightmost module, omit right margin */
        .modules-right > widget:last-child > #workspaces {
            margin-right: 0;
        }

        #pulseaudio.muted {
            background-color: #90b1b1;
            color: #2a5c45;
        }

        #tray > .passive {
            -gtk-icon-effect: dim;
        }

        #tray > .needs-attention {
            -gtk-icon-effect: highlight;
            background-color: #eb4d4b;
        }

        #idle_inhibitor.activated {
            background-color: #ecf0f1;
            color: #2d3436;
        }

        #clock {
            font-weight: bold;
        }

        #scratchpad {
            background: rgba(0, 0, 0, 0.2);
        }

        #scratchpad.empty {
        	background-color: transparent;
        }
      '';
    };
  };

  home.sessionVariablesExtra = ''
    export XDG_CURRENT_DESKTOP=sway
    export QT_QPA_PLATFORMTHEME=kde
  '';
}
