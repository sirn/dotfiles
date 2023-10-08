{ config, pkgs, ... }:

{
  programs.msmtp = {
    enable = true;
  };
}
