{
  config,
  pkgs,
  lib,
  ...
}:

let
  currentZfs = config.boot.zfs.package;
in
{
  options = {
    boot = {
      loader = {
        zfsbootmenu = {
          enable = lib.mkOption {
            type = lib.types.bool;
            default = false;
            description = ''
              Enable support for ZFSBootMenu. Note that you need to install
              ZFSBootMenu and EFI bootloader (such as rEFInd) manually.
            '';
          };

          bootfs = lib.mkOption {
            type = lib.types.str;
            default = "zroot/ROOT/nixos";
            description = ''
              The bootfs dataset where ZFSBootMenu will boot from.
            '';
          };

          keyfile = lib.mkOption {
            type = lib.types.nullOr lib.types.path;
            default = null;
            description = ''
              A keyfile to include in the initramfs image as /etc/zfs/zroot.key.
            '';
          };
        };
      };
    };
  };

  config = lib.mkIf config.boot.loader.zfsbootmenu.enable {
    boot.supportedFilesystems = [ "zfs" ];
    boot.zfs.devNodes = lib.mkDefault "/dev/disk/by-id";
    boot.loader.external = {
      enable = true;
      installHook = pkgs.writeShellScript "zfsbootinstall" ''
        #!{pkgs.bash}/bin/bash
        echo "updating the boot generations directory..."
        ${pkgs.coreutils}/bin/mkdir -p /boot
        bootfs=${config.boot.loader.zfsbootmenu.bootfs}

        declare -A kernels

        kernver() {
          local path=$1
          echo $path | ${pkgs.gnused}/bin/sed -E 's|.*linux-([0-9]+\.[0-9]+\.[0-9]+).*|\1|'
        }

        copyToBoot() {
          local src=$1
          local dstname=$2
          local generation=$3
          local dst=/boot/$dstname-$(kernver "$src")-g$generation

          if ! test -e "$dst"; then
            local dstTmp=$dst.tmp.''$''$
            ${pkgs.coreutils}/bin/cp $src $dstTmp
            ${pkgs.coreutils}/bin/mv $dstTmp $dst
          fi
        }

        addEntry() {
          local path=$1
          local generation=$2

          if ! test -e $path/kernel -a -e $path/initrd; then
            return
          fi

          local kernel=$(${pkgs.coreutils}/bin/readlink -f $path/kernel)
          local initrd=$(${pkgs.coreutils}/bin/readlink -f $path/initrd)

          copyToBoot $kernel vmlinuz $generation
          copyToBoot $initrd initramfs $generation
        }

        updateInit() {
          local dataset=$1
          local init=$2/init

          if ! test -f $init; then
            echo "invalid init path given: $init"
            exit 1
          fi

          local oldval=$(${currentZfs}/bin/zfs get -H org.zfsbootmenu:commandline -o value $dataset)
          local newval

          newval="init=$init ${lib.concatStringsSep " " config.boot.kernelParams}"
          ${currentZfs}/bin/zfs set org.zfsbootmenu:commandline="$newval" $dataset
        }

        # NixOS reads init from cmdline, but ZBM only allows setting
        # cmdline via either ZBM EFI executable or ZFS dataset. This
        # means we can only have a single environment as we need to
        # resolve the path to init in init=... cmdline.
        ${pkgs.coreutils}/bin/rm /boot/vmlinuz-* || true
        ${pkgs.coreutils}/bin/rm /boot/initramfs-* || true

        for generation in $(
          (cd /nix/var/nix/profiles && ${pkgs.coreutils}/bin/ls -d system-*-link) \
          | ${pkgs.gnused}/bin/sed 's/system-\([0-9]\+\)-link/\1/' \
          | ${pkgs.coreutils}/bin/sort -n -r); do
          link=/nix/var/nix/profiles/system-''${generation}-link
          addEntry $link $generation
          updateInit $bootfs $link
          break
        done
      '';
    };

    boot.initrd.secrets =
      if !builtins.isNull config.boot.loader.zfsbootmenu.keyfile then
        {
          "/etc/zfs/zroot.key" = config.boot.loader.zfsbootmenu.keyfile;
        }
      else
        { };
  };
}
