{ nixos-hardware, pkgs, ... }:

{
  imports = [
    ../common.nix
    ../common-zfs.nix

    # machine
    nixos-hardware.nixosModules.lenovo-thinkpad-x1-13th-gen

    # profiles
    ../system/bluetooth.nix
    ../system/desktop.nix
    ../system/game.nix
    ../system/intel-gpu.nix
    ../system/niri.nix
    ../system/pcie-aspm.nix
    ../system/power-management.nix
    ../system/sway.nix
    ../system/wireless.nix

    # programs
    ../programs/1password.nix
    ../programs/appimage.nix

    # services
    ../services/flatpak.nix
    ../services/fwupd.nix
    ../services/greetd.nix
    ../services/node-exporter.nix
    ../services/podman.nix
    ../services/prometheus-agent.nix
    ../services/sanoid.nix
    ../services/thermald.nix
    ../services/tlp.nix
    ../services/upower.nix
  ];

  networking.hostName = "polaris";
  networking.hostId = "1de2954a";

  # Backups
  services.sanoid = {
    datasets = {
      "zroot/DATA" = {
        useTemplate = [ "daily" ];
        processChildrenOnly = true;
        recursive = true;
      };
      "zroot/ROOT" = {
        useTemplate = [ "daily" ];
        processChildrenOnly = true;
        recursive = true;
      };
    };
  };

  # Set default backlight
  services.udev.extraRules = ''
    SUBSYSTEM=="backlight", ACTION=="add", KERNEL=="intel_backlight", ATTR{brightness}="100"
  '';

  # Fix broken audio on Lunar Lake.
  services.pipewire.extraConfig.pipewire."99-lunar-lake-fix" = {
    "context.properties" = {
      "default.clock.min-quantum" = 1024;
      "default.clock.quantum" = 1024;
    };
  };

  # Attempt at fixing ZFS crash on `arc_evict` at `__pgalloc_tag_sub`
  # which seems to be enabled as part of `mem_alloc_profiling_enabled`
  # aka vm.mem_profiling.
  #
  # Not sure if it's a bug in ZFS or the kernel. Let's disable this
  # for the time being and see if it will fix the issue. So far, this
  # crash is observed on ThinkPad X1 Carbon Gen 13 (Intel Core 258V).
  #
  # TODO: reevaluate after 6.17 or if the crash happens again
  boot.kernel.sysctl = {
    "vm.mem_profiling" = "0";
  };
}
