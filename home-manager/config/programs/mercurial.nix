{ config, lib, ... }:

let
  primaryAccount = lib.findFirst (a: a.primary) null (lib.attrValues config.accounts.email.accounts);
in
{
  programs.mercurial = {
    enable = true;

    userName = primaryAccount.realName;
    userEmail = primaryAccount.address;
    ignores = config.programs.git.ignores;
  };
}
