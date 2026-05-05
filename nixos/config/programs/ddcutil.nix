{ pkgs, ... }:

{
  hardware.i2c.enable = true;

  services.udev.packages = [ pkgs.ddcutil ];

  environment.systemPackages = [ pkgs.ddcutil ];
}
