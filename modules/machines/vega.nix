{
  desktop.enable = true;
  flatpak.enable = true;

  imports = [
    ../common.nix
    ../common-linux.nix

    # services
    ../services/syncthing.nix
    ../services/xlocate.nix
  ];
}
