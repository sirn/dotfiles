{ config, pkgs, ... }:

let
  inherit (config.home) homeDirectory;
in
{
  accounts.email.maildirBasePath = "${homeDirectory}/Mail";

  accounts.email.accounts = {
    ogsite = {
      primary = true;
      address = "sirn@ogsite.net";
      userName = "sirn@fastmail.fm";
      realName = "Sirn Thanabulpong";
      passwordCommand = "${pkgs.pass}/bin/pass mail/sirn@fastmail.fm";

      imap = {
        host = "imap.fastmail.com";
        tls = {
          enable = true;
          useStartTls = false;
        };
      };

      smtp = {
        host = "smtp.fastmail.com";
        port = 465;
        tls = {
          enable = true;
          useStartTls = false;
        };
      };

      folders = {
        drafts = "Drafts";
        inbox = "INBOX";
        sent = "Sent Items";
        trash = "Trash";
      };

      maildir = {
        path = "Ogsite";
      };

      notmuch = {
        enable = true;
      };

      mbsync = {
        enable = true;
        create = "both";
        expunge = "both";
        flatten = ".";
        patterns = [
          "INBOX"
          "Archive"
          "Drafts"
          "Sent Items"
          "Junk Mail"
          "Trash"
          "Feed"
          "Lists"
          "Messages"
          "Noreply"
        ];
      };

      msmtp = {
        enable = true;
      };
    };
  };
}
