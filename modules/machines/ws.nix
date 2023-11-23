{
  machine.nixos.enable = true;

  imports = [
    ../common.nix
    ../common-linux.nix
  ];
}
