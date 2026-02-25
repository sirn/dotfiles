{ lib, pkgs, ... }:

{
  home.packages = [
    (if pkgs.stdenv.isDarwin
    then pkgs.ghostty-bin.terminfo
    else pkgs.ghostty.terminfo)
  ];
}
