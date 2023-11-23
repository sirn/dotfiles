{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    curlFull
  ];
}
