{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
in
{
  imports = [
    ../programs/fuzzel.nix
    ../programs/kanshi.nix
    ../programs/mako.nix
    ../programs/sway-audio-idle-inhibit.nix
    ../programs/sway.nix
    ../programs/swayidle.nix
    ../programs/swaylock.nix
    ../programs/waybar.nix
    ../programs/wayexec.nix
    ../programs/wlsunset.nix
    ../programs/xdg-portal.nix
  ];

  i18n.inputMethod = mkIf config.machine.isNixOS {
    enabled = "fcitx5";

    fcitx5 = {
      addons = with pkgs; [
        fcitx5-mozc
      ];
    };
  };

  wayland.windowManager.sway = {
    config = {
      seat = {
        "*" = {
          xcursor_theme = "${config.gtk.cursorTheme.name} ${toString config.gtk.cursorTheme.size}";
        };
      };
    };
  };

  # non-NixOS; assume no systemd
  wayexec.services = {
    fcitx5 = mkIf (config.i18n.inputMethod.enabled != "fcitx5") {
      runScript = ''
        #!${pkgs.execline}/bin/execlineb
        fdmove -c 2 1
        fcitx5 -D -r
      '';
    };

    pipewire = mkIf (!config.machine.isNixOS) {
      runScript = ''
        #!${pkgs.execline}/bin/execlineb
        fdmove -c 2 1
        pipewire
      '';
    };
  };

  gtk = {
    enable = true;
    cursorTheme.name = "breeze_cursors";
    cursorTheme.size = 24;
    font.name = "sans-serif";
    font.size = 10;
    theme.name = "Breeze";
    iconTheme.name = "Breeze";

    gtk2.extraConfig = ''
      gtk-alternative-button-order = 1;
    '';

    gtk3.extraConfig =
      {
        gtk-alternative-button-order = 1;
      };

    gtk4.extraConfig =
      {
        document-font-name = "sans-serif 10";
        monospace-font-name = "monospace 10";
      };
  };

  # Setting qt platformTheme and style via Home Manager on non-NixOS
  # can cause SEGFAULT due to dependency mismatch.
  qt = {
    enable = config.machine.isNixOS;
    platformTheme = {
      name = "kde";
    };
    style = {
      name = "breeze";
    };
  };

  home = mkIf config.machine.isNixOS
    {
      packages = with pkgs; [
        breeze-qt5
        breeze-gtk
        breeze-icons
        hicolor-icon-theme
      ];
    } // (
    mkIf (!config.machine.isNixOS) {
      sessionVariables = {
        QT_QPA_PLATFORMTHEME = config.qt.platformTheme;
        QT_STYLE_OVERRIDE = config.qt.style.name;
      };
    });
}
