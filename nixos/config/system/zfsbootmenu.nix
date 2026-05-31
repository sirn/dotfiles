{ pkgs, ... }:

{
  boot.kernelPackages = pkgs.linuxPackages_6_18;

  boot.zfs.package = pkgs.zfs_2_4;

  # Tell systemd-fstab-generator the root filesystem type and options explicitly.
  # Without these, ZBM's `root=zfs:...` cmdline param can confuse
  # systemd v259+ into overriding the fstab-generated sysroot.mount
  # with wrong Type=nfs and Options=ro (dropping the fstab's zfsutil).
  boot.kernelParams = [
    "rootfstype=zfs"
    "rootflags=zfsutil"
  ];
  boot.zfs.forceImportRoot = false;

  boot.loader.zfsbootmenu = {
    enable = true;
  };
}
