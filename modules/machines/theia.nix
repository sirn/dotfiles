{
  desktop.enable = true;
  launchd.enable = true;

  imports = [
    ../common.nix
    ../common-darwin.nix
  ];
}
