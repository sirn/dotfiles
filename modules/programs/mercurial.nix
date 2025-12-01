{ config, pkgs, ... }:

{
  programs.mercurial = {
    enable = true;

    userName = config.programs.git.settings.user.name;
    userEmail = config.programs.git.settings.user.email;
    ignores = config.programs.git.ignores;
  };
}
