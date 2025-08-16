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
    ../services/xwayland-satellite.nix
  ];

  # niri-flake is enabling gnome-keyring by default but gnome-keyring is gross.
  services.gnome-keyring.enable = lib.mkForce false;

  # If we're running on nixOS, let's also attempt to start kdewallet
  # alongside with the session; this should already be unlocked via PAM
  programs.niri.settings = {
    spawn-at-startup =
      if config.machine.isNixOS
      then [{ command = [ "${pkgs.kdePackages.kwallet-pam}/libexec/pam_kwallet_init" ]; }]
      else [ ];
  };
}
