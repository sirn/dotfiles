{ pkgs, ... }:

{
  boot.kernelPackages = pkgs.linuxPackages_6_18;

  boot.zfs.package = pkgs.zfs_2_4;

  boot.loader.zfsbootmenu = {
    enable = true;
  };
}
