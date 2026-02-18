{ pkgs, ... }:

{
  home.packages = with pkgs; [
    ghostty.terminfo
  ];
}
