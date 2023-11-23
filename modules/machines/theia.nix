{
  launchd.enable = true;
  machine.gui.enable = true;

  imports = [
    ../common.nix
    ../common-darwin.nix

    # services
    ../services/syncthing.nix
  ];
}
