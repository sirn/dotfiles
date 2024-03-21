{ pkgs, ... }:

{
  home.packages = with pkgs; [
    local.localias
  ];
}
