{ pkgs, ... }:

{
  home.packages = with pkgs; [
    local.tincan
  ];
}
