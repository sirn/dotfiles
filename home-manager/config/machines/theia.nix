{
  launchd.enable = true;

  imports = [
    ../common.nix
    ../common-darwin.nix

    ../programs/aerospace.nix
  ];
}
