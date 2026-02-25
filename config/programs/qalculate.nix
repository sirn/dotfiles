{ pkgs, ... }:

{
  home.packages = with pkgs; [
    libqalculate
  ];
}
