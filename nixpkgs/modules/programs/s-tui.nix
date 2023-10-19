{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    local.s-tui
  ];
}
