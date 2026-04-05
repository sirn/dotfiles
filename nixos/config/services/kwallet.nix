{
  config,
  pkgs,
  lib,
  ...
}:

let
  plasmacfg = config.services.desktopManager.plasma6;

  greetdcfg = config.services.greetd;
in
{
  environment.systemPackages = with pkgs; [ kdePackages.kwallet ];

  security.pam.services = with pkgs; {
    "login" = {
      kwallet = lib.mkIf plasmacfg.enable {
        enable = true;
        forceRun = true;
        package = kdePackages.kwallet-pam;
      };
    };
    "greetd" = {
      kwallet = lib.mkIf greetdcfg.enable {
        enable = true;
        forceRun = true;
        package = kdePackages.kwallet-pam;
      };
    };
  };
}
