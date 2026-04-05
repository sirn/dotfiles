{ config, lib, ... }:

{
  virtualisation.podman = {
    enable = true;

    autoPrune = {
      enable = config.virtualisation.podman.enable;
      dates = "daily";
    };

    dockerCompat = config.virtualisation.podman.enable && !config.virtualisation.docker.enable;
  };
}
