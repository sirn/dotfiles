{ config, lib, pkgs, ... }:

{
  programs.mouseless = {
    enable = true;
    launchd.enable = true;
  };
}
