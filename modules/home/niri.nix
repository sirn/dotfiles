{ config, lib, pkgs, ... }:

{
  imports = [
    ../programs/fuzzel.nix
    ../programs/niri.nix
    ../programs/waybar.nix
    ../programs/xdg-portal.nix

    ../services/kanshi.nix
    ../services/mako.nix
    ../services/sway-audio-idle-inhibit.nix
    ../services/swayidle.nix
    ../services/swaylock.nix
    ../services/wlsunset.nix
  ];
}
