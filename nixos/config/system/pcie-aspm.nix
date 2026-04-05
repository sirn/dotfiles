{
  config,
  lib,
  pkgs,
  ...
}:

{
  boot.kernelParams = [
    "pcie_aspm=force"
    "pcie_aspm.policy=powersupersave"
  ];
}
