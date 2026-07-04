{
  config,
  lib,
  pkgs,
  ...
}:

let
  tlpBin = lib.getExe' config.services.tlp.package "tlp";
  systemdRun = lib.getExe' pkgs.systemd "systemd-run";
in
{
  assertions = [
    {
      assertion = !config.services.power-profiles-daemon.enable;
      message = "services.tlp is enabled alongside services.power-profiles-daemon; choose only one.";
    }
    {
      assertion = !config.services.powerband.enable;
      message = "services.tlp is enabled alongside services.powerband; choose only one.";
    }
  ];

  services.tlp = {
    enable = true;
    package = pkgs.tlp;
    pd = {
      enable = true;
      package = pkgs.tlp-pd;
    };
    settings = {
      # Disable TLP's built-in auto-switching (which hardcodes AC→performance,
      # BAT→balanced).  With auto-switching disabled, 'tlp auto' keeps the last
      # profile, and the udev rules below take over to set the correct profile
      # per power source.
      TLP_AUTO_SWITCH = 0;

      # Power profile used by 'tlp start' at boot.  Our udev rules will set the
      # correct profile on power source changes, but this ensures a sensible
      # boot default (balanced is correct for AC and close for BAT).
      TLP_DEFAULT_MODE = "BAL";

      # TLP maps each power profile to a platform (ACPI) profile:
      #   performance → PLATFORM_PROFILE_ON_AC
      #   balanced    → PLATFORM_PROFILE_ON_BAT
      #   power-saver → PLATFORM_PROFILE_ON_SAV
      #
      # With our udev rules the active power profiles are:
      #   AC  → balanced    → platform profile = "balanced"
      #   BAT → power-saver → platform profile = "low-power"
      PLATFORM_PROFILE_ON_AC = "performance";
      PLATFORM_PROFILE_ON_BAT = "balanced";
      PLATFORM_PROFILE_ON_SAV = "low-power";

      CPU_HWP_DYN_BOOST_ON_SAV = 0;
      RUNTIME_PM_ON_BAT = "auto";
      SATA_LINKPWR_ON_BAT = "min_power";
      USB_AUTOSUSPEND = 1;
      WIFI_PWR_ON_BAT = "on";
      START_CHARGE_THRESH_BAT0 = 75;
      STOP_CHARGE_THRESH_BAT0 = 80;
    };
  };

  # Override TLP's default power profiles via udev rules:
  #   AC  → balanced    (TLP default: performance)
  #   BAT → power-saver (TLP default: balanced)
  #
  # TLP's own udev rule calls 'tlp auto', which with TLP_AUTO_SWITCH=0 keeps the
  # last profile.  These rules then set the desired profile.  systemd-run
  # --collect runs the command asynchronously (after 'tlp auto' finishes) to
  # avoid lock contention.
  services.udev.extraRules = ''
    SUBSYSTEM=="power_supply", KERNEL=="AC*",  ACTION=="change", ENV{POWER_SUPPLY_ONLINE}=="1", RUN+="${systemdRun} --collect ${tlpBin} balanced"
    SUBSYSTEM=="power_supply", KERNEL=="AC*",  ACTION=="change", ENV{POWER_SUPPLY_ONLINE}=="0", RUN+="${systemdRun} --collect ${tlpBin} power-saver"
    SUBSYSTEM=="power_supply", KERNEL=="ADP*", ACTION=="change", ENV{POWER_SUPPLY_ONLINE}=="1", RUN+="${systemdRun} --collect ${tlpBin} balanced"
    SUBSYSTEM=="power_supply", KERNEL=="ADP*", ACTION=="change", ENV{POWER_SUPPLY_ONLINE}=="0", RUN+="${systemdRun} --collect ${tlpBin} power-saver"
  '';
}
