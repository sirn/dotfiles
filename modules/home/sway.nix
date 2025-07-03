{ config, lib, pkgs, ... }:

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
  i18n.inputMethod = lib.mkIf config.machine.isNixOS {
    enable = true;
    type = "fcitx5";

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

      # If we're running on nixOS, let's also attempt to start kdewallet
      # alongside with the session; this should already be unlocked via PAM
      startup =
        if config.machine.isNixOS
        then [{ command = "${pkgs.kdePackages.kwallet-pam}/libexec/pam_kwallet_init"; }]
        else [ ];
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

  # On a non-NixOS, we just provide the proper environment variables
  # for it to pick up the correct themes installed with the system
  home = lib.mkIf (!config.machine.isNixOS) {
    sessionVariables = {
      QT_QPA_PLATFORMTHEME = config.qt.platformTheme.name;
      QT_STYLE_OVERRIDE = config.qt.style.name;
    };
  };
}
