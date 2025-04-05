{ config, pkgs, lib, ... }:

{
  services.gpg-agent = {
    enable = config.programs.gpg.enable;

    pinentryPackage = lib.mkDefault (
      if pkgs.stdenv.isDarwin
      then pkgs.pinentry_mac
      else pkgs.pinentry-curses
    );

    defaultCacheTtl = 86400;
    maxCacheTtl = 86400;

    extraConfig = ''
      allow-emacs-pinentry
      allow-loopback-pinentry
    '';
  };
}
