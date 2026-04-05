{
  nixos-hardware,
  pkgs,
  lib,
  ...
}:

let
  systemdRun = lib.getExe' pkgs.systemd "systemd-run";
  system76Power = lib.getExe pkgs.system76-power;
in
{
  imports = [
    ../common.nix

    # profiles
    ../system/bluetooth.nix
    ../system/desktop.nix
    ../system/game.nix
    ../system/intel-gpu.nix
    ../system/microvm.nix
    ../system/niri.nix
    ../system/pcie-aspm.nix
    ../system/plymouth.nix
    ../system/power-management.nix
    ../system/sway.nix
    ../system/wireless.nix

    # programs
    ../programs/appimage.nix

    # services
    ../services/flatpak.nix
    ../services/greetd.nix
    ../services/node-exporter.nix
    ../services/podman.nix
    ../services/prometheus.nix
    ../services/udisks2.nix
    ../services/upower.nix

    # hardware
    "${nixos-hardware}/common/cpu/intel"
    "${nixos-hardware}/common/cpu/intel/meteor-lake"
    "${nixos-hardware}/common/gpu/intel/meteor-lake"
  ];

  networking.hostName = "system76";

  # Bootloader
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";

  # Power Management
  hardware.system76.enableAll = true;
  services.thermald.enable = true;
  powerManagement.powertop.enable = true;

  # Limit battery charge to 75-80% for battery longevity
  systemd.services.system76-battery-limit = {
    description = "Set System76 battery charge threshold to 80%";
    after = [ "system76-power.service" ];
    wants = [ "system76-power.service" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${pkgs.system76-power}/bin/system76-power charge-thresholds 75 80";
    };
    wantedBy = [ "multi-user.target" ];
  };

  # Automatic power profile switching based on AC state
  services.udev.extraRules = ''
    # AC plugged in - switch to balanced profile
    SUBSYSTEM=="power_supply", KERNEL=="AC*",  ACTION=="change", ENV{POWER_SUPPLY_ONLINE}=="1", RUN+="${systemdRun} --collect ${system76Power} profile balanced"
    SUBSYSTEM=="power_supply", KERNEL=="ADP*", ACTION=="change", ENV{POWER_SUPPLY_ONLINE}=="1", RUN+="${systemdRun} --collect ${system76Power} profile balanced"

    # On battery - switch to battery profile
    SUBSYSTEM=="power_supply", KERNEL=="AC*",  ACTION=="change", ENV{POWER_SUPPLY_ONLINE}=="0", RUN+="${systemdRun} --collect ${system76Power} profile battery"
    SUBSYSTEM=="power_supply", KERNEL=="ADP*", ACTION=="change", ENV{POWER_SUPPLY_ONLINE}=="0", RUN+="${systemdRun} --collect ${system76Power} profile battery"
  '';

  boot.kernelParams = [
    "i915.enable_guc=3"
    "i915.enable_fbc=1"
    "i915.enable_psr=1"
    "consoleblank=60"
  ];

  # Fix broken audio with BTD600/BTD700
  services.pipewire.extraConfig.pipewire."99-btd600-btd700-fix" = {
    "context.properties" = {
      "default.clock.min-quantum" = 1024;
      "default.clock.quantum" = 1024;
    };
  };
}
