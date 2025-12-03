{ lib, config, pkgs, ... }:

{
  programs.ghostty = {
    enable = true;
    package = config.lib.nixGL.wrap pkgs.unstable.ghostty;
    settings = {
      font-family = "PragmataPro Mono Liga";

      command = builtins.concatStringsSep " " (
        [ config.machine.interactiveShell ]
        ++ lib.optional pkgs.stdenv.isDarwin "--login"
      );

      font-size =
        if pkgs.stdenv.isDarwin
        then 14
        else 12;
    };
  };
}
