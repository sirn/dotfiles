{
  launchd.enable = true;

  imports = [
    ../profiles/common.nix
    ../profiles/devops.nix
    ../profiles/kubeops.nix
    ../profiles/dev.nix

    # services
    ../launchd/userenv.nix
  ];
}
