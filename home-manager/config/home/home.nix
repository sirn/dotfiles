{
  config,
  pkgs,
  lib,
  ...
}:

{
  nix.gc = {
    automatic = true;
  };

  targets.genericLinux.nixGL.packages =
    if pkgs.stdenv.isLinux && config.targets.genericLinux.enable then pkgs.nixgl else null;

  systemd = lib.mkIf config.systemd.user.enable {
    user = {
      startServices = "sd-switch";
    };
  };
}
