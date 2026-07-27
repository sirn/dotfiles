{ config, lib, ... }:

let
  enabledAccounts = lib.filter (a: a.enable) (lib.attrValues config.accounts.email.accounts);
in
{
  accounts.email.maildirBasePath = lib.mkDefault "${config.home.homeDirectory}/Mail";

  accounts.email.accounts.ogsite = {
    primary = lib.mkDefault true;
    address = lib.mkDefault "sirn@ogsite.net";
    userName = lib.mkDefault "sirn@fastmail.fm";
    realName = lib.mkDefault "Sirn Thanabulpong";

    imap = {
      host = lib.mkDefault "imap.fastmail.com";
      port = lib.mkDefault 993;
    };

    smtp = {
      host = lib.mkDefault "smtp.fastmail.com";
      port = lib.mkDefault 465;
    };
  };

  # Activate the global programs only when at least one enabled account
  # opts into them, so machines without credentials stay sync-free.
  programs.mbsync.enable = lib.mkDefault (lib.any (a: a.mbsync.enable) enabledAccounts);
  programs.msmtp.enable = lib.mkDefault (lib.any (a: a.msmtp.enable) enabledAccounts);
}
