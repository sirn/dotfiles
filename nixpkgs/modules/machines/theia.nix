{
  machine.gui.enable = true;
  launchd.enable = true;

  imports = [
    ../profiles/common.nix
    ../profiles/dev.nix
    ../profiles/devops.nix
    ../profiles/graphical.nix
    ../profiles/multimedia.nix

    # services
    ../launchd/userenv.nix
    ../launchd/emacs.nix
  ];
}
