{ config, ... }:

{
  accounts.email.maildirBasePath = "${config.home.homeDirectory}/Mail";

  accounts.email.accounts = {
    ogsite = {
      primary = true;
      address = "sirn@ogsite.net";
      userName = "sirn@fastmail.fm";
      realName = "Sirn Thanabulpong";
    };
  };
}
