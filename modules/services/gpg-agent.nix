{ config, pkgs, lib, ... }:

{
  services.gpg-agent = {
    enable = config.programs.gpg.enable;
    enableSshSupport = true;

    pinentry = {
      package = lib.mkDefault (
        if pkgs.stdenv.isDarwin
        then pkgs.pinentry_mac
        else pkgs.pinentry-curses
      );
    };

    defaultCacheTtl = 21600;
    defaultCacheTtlSsh = 21600;
    maxCacheTtl = 43200;
    maxCacheTtlSsh = 43200;

    # gpg --list-keys --with-keygrip
    sshKeys = [
      "DB2F6C327247DA184010A3181710D1C56B427410"
    ];

    extraConfig = ''
      allow-emacs-pinentry
      allow-loopback-pinentry
    '';
  };
}
