{ pkgs, ... }:

{
  programs.gamemode = {
    enable = true;
    enableRenice = true;

    settings = {
      general = {
        renice = 10;
      };
    };
  };
}
