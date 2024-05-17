{ pkgs, config, ... }:

{
  desktop.enable = true;
  flatpak.enable = true;

  imports = [
    ../common.nix
    ../common-linux.nix

    # services
    ../services/dropbox.nix
  ];
}
