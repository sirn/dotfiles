{ config, lib, pkgs, ... }:

{
  imports = [
    ../programs/fuzzel.nix
    ../programs/niri.nix
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
  ];

  # niri-flake is enabling gnome-keyring by default but gnome-keyring is gross.
  services.gnome-keyring.enable = lib.mkForce false;
}
