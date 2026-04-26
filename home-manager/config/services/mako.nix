{
  config,
  lib,
  pkgs,
  ...
}:

{
  services.mako = {
    enable = true;

    settings = {
      border-size = 4;
      border-radius = 4;
    };
  };
}
