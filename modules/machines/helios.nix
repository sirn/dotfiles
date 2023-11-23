{
  flatpak.enable = true;
  machine.gui.enable = true;
  runit.enable = true;

  imports = [
    ../common.nix
    ../common-linux.nix

    # services
    ../services/syncthing.nix
    ../services/xlocate.nix
  ];
}
