{
  machine.gui.enable = true;

  imports = [
    ../common.nix

    # profile
    ../home/launchd.nix
    ../home/fonts.nix

    # programs
    ../programs/amethyst.nix
    ../programs/emacsc.nix
    ../programs/intellij.nix
    ../programs/mpv.nix

    # services
    ../services/emacs.nix
    ../services/syncthing.nix

    # launchd specific
    ../launchd/userenv.nix
  ];
}
