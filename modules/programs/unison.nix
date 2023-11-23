{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    local.unison-nox
  ];
}
