{
  machine.isNixOS = true;

  imports = [
    ../common.nix
    ../common-linux.nix
  ];
}
