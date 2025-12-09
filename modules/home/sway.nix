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
  ];
}
