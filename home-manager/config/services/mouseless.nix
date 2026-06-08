{
  config,
  lib,
  pkgs,
  ...
}:

{
  services.mouseless = {
    enable = true;
    launchd.enable = true;
  };
}
