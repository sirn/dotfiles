{
  machine.gui.enable = true;

  imports = [
    ../common.nix
    ../common-macos.nix

    # services
    ../services/syncthing.nix
  ];
}
