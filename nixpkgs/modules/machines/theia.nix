{
  machine.gui.enable = true;

  imports = [
    ../profiles/common.nix
    ../profiles/dev.nix
    ../profiles/graphical.nix
    ../profiles/multimedia.nix

    # machine-specific
    ../home/launchd.nix

    # services
    ../launchd/userenv.nix
    ../launchd/emacs.nix
  ];
}
