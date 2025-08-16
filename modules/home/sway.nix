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
    ../services/xwayland-satellite.nix
  ];

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
}
