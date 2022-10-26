{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    nomad
  ];
}
