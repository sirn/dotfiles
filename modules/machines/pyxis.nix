{
  machine.gui.enable = true;

  imports = [
    ../common.nix

    # profile
    ../home/fonts.nix

    # programs
    ../programs/amethyst.nix
    ../programs/intellij.nix
    ../programs/mpv.nix

    # services
    ../services/syncthing.nix
  ];
}
