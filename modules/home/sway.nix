{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf;
in
{
  imports = [
    ../programs/fuzzel.nix
    ../programs/sway.nix
    ../programs/waybar.nix
    ../programs/xdg-portal.nix

    ../services/kanshi.nix
    ../services/mako.nix
    ../services/sway-audio-idle-inhibit.nix
    ../services/swayidle.nix
    ../services/swaylock.nix
    ../services/wlsunset.nix
  ];

  # On non-NixOS, this should be installed using OS package manager.
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

  qt = {
    # Setting qt platformTheme and style via Home Manager on non-NixOS
    # can cause SEGFAULT due to dependency mismatch.
    enable = config.machine.isNixOS;

    platformTheme = {
      name = "kde";
    };
    style = {
      name = "breeze";
    };
  };

  # Installing themes through Home Manager on NixOS can cause errors
  # due to some of these themes require a matching system libraries
  home = mkIf config.machine.isNixOS
    {
      packages = with pkgs; [
        breeze-qt5
        breeze-gtk
        breeze-icons
        hicolor-icon-theme
      ];
    } // (
    # On a non-NixOS, we just provide the proper environment variables
    # for it to pick up the correct themes installed with the system
    mkIf (!config.machine.isNixOS) {
      sessionVariables = {
        QT_QPA_PLATFORMTHEME = config.qt.platformTheme;
        QT_STYLE_OVERRIDE = config.qt.style.name;
      };
    });
}
