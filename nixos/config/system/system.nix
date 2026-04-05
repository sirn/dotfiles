{ lib, pkgs, ... }:

{
  time.timeZone = "Asia/Tokyo";
  boot.kernelPackages = lib.mkDefault pkgs.linuxPackages_6_18;
  services.logind.settings.Login.KillUserProcesses = true;
  hardware.enableRedistributableFirmware = lib.mkDefault true;
  nix.package = pkgs.nixVersions.latest;
}
