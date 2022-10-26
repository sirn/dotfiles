{
  launchd.enable = true;

  imports = [
    ../profiles/common.nix
    ../profiles/devops.nix
    ../profiles/dev.nix

    # services
    ../launchd/userenv.nix
    ../launchd/emacs.nix
  ];
}
