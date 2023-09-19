{ config, pkgs, ... }:

{
  programs.mercurial = {
    enable = true;

    userName = config.programs.git.userName;
    userEmail = config.programs.git.userEmail;
    ignores = config.programs.git.ignores;
  };
}
