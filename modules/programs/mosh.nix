{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    mosh
  ];
}
