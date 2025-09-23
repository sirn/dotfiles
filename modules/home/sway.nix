{ config, lib, pkgs, ... }:

{
  imports = [
    ../programs/fuzzel.nix
    ../programs/sway.nix
    ../programs/waybar.nix
    ../programs/xdg-portal.nix

    ../services/kanshi.nix
    ../services/kwallet.nix
    ../services/mako.nix
    ../services/sway-audio-idle-inhibit.nix
    ../services/swayidle.nix
    ../services/swaylock.nix
    ../services/swww.nix
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
    };
  };
}
