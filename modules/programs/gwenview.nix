{ pkgs, ... }:

{
  home.packages = with pkgs; [
    kdePackages.gwenview
  ];
}
