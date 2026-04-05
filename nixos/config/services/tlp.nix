{ config, lib, ... }:

{
  assertions = [
    {
      assertion = !config.services.power-profiles-daemon.enable;
      message = "services.tlp is enabled alongside services.power-profiles-daemon; choose only one.";
    }
  ];

  services.tlp = {
    enable = true;
    settings = {
      CPU_BOOST_ON_BAT = 0;
      CPU_ENERGY_PERF_POLICY_ON_BAT = "balance_power";
      RUNTIME_PM_ON_BAT = "auto";
      SATA_LINKPWR_ON_BAT = "min_power";
      USB_AUTOSUSPEND = 1;
      WIFI_PWR_ON_BAT = "on";
      START_CHARGE_THRESH_BAT0 = 75;
      STOP_CHARGE_THRESH_BAT0 = 80;
    };
  };
}
