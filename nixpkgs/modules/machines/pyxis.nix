{
  machine.gui.enable = true;
  launchd.enable = true;

  imports = [
    ../profiles/common.nix
    ../profiles/dev.nix
    ../profiles/devops.nix

    # services
    ../launchd/userenv.nix
    ../launchd/emacs.nix
  ];
}
