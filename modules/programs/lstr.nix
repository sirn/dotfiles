{ pkgs, ... }:

{
  home.packages = with pkgs; [
    unstable.lstr
  ];
}
