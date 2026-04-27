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

  home.file.".face".source = pkgs.fetchurl {
    url = "https://avatars.githubusercontent.com/u/4061";
    sha256 = "sha256-Dldkh1eXoxfon0mmNpMYsEo8YSWL29E1+v5V5UyATi8=";
  };

  targets.genericLinux.nixGL.packages =
    if pkgs.stdenv.isLinux && config.targets.genericLinux.enable then pkgs.nixgl else null;

  systemd = lib.mkIf config.systemd.user.enable {
    user = {
      startServices = "sd-switch";
    };
  };
}
