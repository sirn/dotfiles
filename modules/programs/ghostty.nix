{ config, pkgs, ... }:

{
  programs.ghostty = {
    enable = true;
    package = config.lib.nixGL.wrap pkgs.unstable.ghostty;
    settings = {
      font-family = "PragmataPro Mono Liga";

      font-size =
        if pkgs.stdenv.isDarwin
        then 14
        else 12;
    };
  };
}
