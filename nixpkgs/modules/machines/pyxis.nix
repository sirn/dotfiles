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
    ../launchd/userenv.nix
    ../launchd/emacs.nix
  ];
}
