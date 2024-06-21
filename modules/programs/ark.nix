{ pkgs, ... }:

{
  home.packages = with pkgs; [
    kdePackages.ark
  ];
}
