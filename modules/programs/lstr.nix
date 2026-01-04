{ pkgs, ... }:

{
  home.packages = with pkgs; [
    lstr
  ];
}
