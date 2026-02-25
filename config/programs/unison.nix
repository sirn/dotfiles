{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    (unison.override {
      enableX11 = false;
    })
  ];
}
