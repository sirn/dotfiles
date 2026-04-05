{
  config,
  lib,
  pkgs,
  ...
}:

let
  hasGui = config.services.xserver.enable || config.programs.uwsm.enable;
in
{
  hardware.nvidia = {
    package = config.boot.kernelPackages.nvidiaPackages.stable;
    nvidiaSettings = hasGui;
    open = true;

    powerManagement.enable = false;
    powerManagement.finegrained = false;
    modesetting.enable = hasGui;

    nvidiaPersistenced = !hasGui;
  };

  services.xserver.videoDrivers = [
    "nvidia"
  ]
  ++ lib.optionals hasGui [
    "modesetting"
    "fbdev"
  ];

  environment.systemPackages = [ config.hardware.nvidia.package.bin ];
}
