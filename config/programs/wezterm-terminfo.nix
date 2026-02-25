{ pkgs, ... }:

{
  home.packages = [
    pkgs.wezterm.terminfo
  ];
}
