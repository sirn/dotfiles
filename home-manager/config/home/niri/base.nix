{ lib, ... }:

{
  imports = [
    ../../programs/niri.nix
    ../../programs/xdg-portal.nix

    # Services
    ../../services/kanshi.nix
    ../../services/kwallet.nix
    ../../services/udiskie.nix
  ];

  # niri-flake is enabling gnome-keyring by default but gnome-keyring is gross.
  services.gnome-keyring.enable = lib.mkForce false;
}
