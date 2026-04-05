{
  config,
  lib,
  pkgs,
  ...
}:

let
  hasGui = config.services.xserver.enable || config.programs.uwsm.enable;
  hasContainers = config.virtualisation.docker.enable || config.virtualisation.podman.enable;
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

  hardware.nvidia-container-toolkit.enable = hasContainers;

  services.xserver.videoDrivers = [
    "nvidia"
  ]
  ++ lib.optionals hasGui [
    "modesetting"
    "fbdev"
  ];

  environment.systemPackages = [ config.hardware.nvidia.package.bin ];
}
