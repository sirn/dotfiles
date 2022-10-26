{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    tcl
    tcllib
    tcltls
  ];
}
