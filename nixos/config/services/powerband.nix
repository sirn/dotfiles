{
  config,
  lib,
  ...
}:

{
  assertions = [
    {
      assertion = !config.services.tlp.enable;
      message = "services.powerband is enabled alongside services.tlp; choose only one.";
    }
    {
      assertion = !config.services.power-profiles-daemon.enable;
      message = "services.powerband is enabled alongside services.power-profiles-daemon; choose only one.";
    }
  ];

  services.powerband.enable = true;
}
