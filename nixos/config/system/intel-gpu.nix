{ config, pkgs, ... }:

{
  hardware.graphics = {
    extraPackages = with pkgs; [
      intel-media-driver
      intel-vaapi-driver
      libvdpau-va-gl
      vpl-gpu-rt
    ];
  };
}
