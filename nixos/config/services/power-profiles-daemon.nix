{
  config,
  lib,
  pkgs,
  ...
}:

let
  plasmacfg = config.services.desktopManager.plasma6;

  cfg = config.services.power-profiles-daemon;

  systemdRun = lib.getExe' pkgs.systemd "systemd-run";

  ppdExe = lib.getExe cfg.package;
in
{
  assertions = [
    {
      assertion = !config.services.tlp.enable;
      message = "services.power-profiles-daemon is enabled alongside services.tlp; choose only one.";
    }
    {
      assertion = !config.services.powerband.enable;
      message = "services.power-profiles-daemon is enabled alongside services.powerband; choose only one.";
    }
  ];

  services.power-profiles-daemon = {
    enable = true;
  };

  services.udev.extraRules = lib.optionalString (!plasmacfg.enable) ''
    SUBSYSTEM=="power_supply", KERNEL=="AC*",  ACTION=="change", ENV{POWER_SUPPLY_ONLINE}=="1", RUN+="${systemdRun} --collect ${ppdExe} set balanced"
    SUBSYSTEM=="power_supply", KERNEL=="AC*",  ACTION=="change", ENV{POWER_SUPPLY_ONLINE}=="0", RUN+="${systemdRun} --collect ${ppdExe} set power-saver"
    SUBSYSTEM=="power_supply", KERNEL=="ADP*", ACTION=="change", ENV{POWER_SUPPLY_ONLINE}=="1", RUN+="${systemdRun} --collect ${ppdExe} set balanced"
    SUBSYSTEM=="power_supply", KERNEL=="ADP*", ACTION=="change", ENV{POWER_SUPPLY_ONLINE}=="0", RUN+="${systemdRun} --collect ${ppdExe} set power-saver"
  '';
}
