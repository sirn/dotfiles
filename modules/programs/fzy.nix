{ pkgs, ... }:

{
  home.packages = with pkgs; [
    fzy
  ];
}
