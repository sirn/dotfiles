{ lib, pkgs, ... }:

let
  breezePkg = pkgs.kdePackages.breeze;
in
{
  imports = [
    ./breeze.nix
  ];

  gtk = {
    theme = {
      name = "Breeze-Dark";
    };

    iconTheme = {
      name = "breeze-dark";
    };
  };

  xdg.configFile = {
    "kdeglobals" = {
      source = "${breezePkg}/share/color-schemes/BreezeDark.colors";
    };
  };
}
