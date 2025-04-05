{ pkgs, lib, ... }:

{
  services.gpg-agent = {
    enable = true;

    pinentryPackage = lib.mkDefault (
      if pkgs.stdenv.isDarwin
      then pkgs.pinentry-mac
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
