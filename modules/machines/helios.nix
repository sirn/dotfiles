{
  machine.runit.enable = true;
  machine.gui.enable = true;

  imports = [
    ../common.nix
    ../common-linux-desktop.nix

    # services
    ../services/syncthing.nix
    ../services/xlocate.nix
  ];
}
