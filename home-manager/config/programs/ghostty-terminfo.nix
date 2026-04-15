{ lib, pkgs, ... }:

{
  home.packages = [
    (
      if pkgs.stdenv.isDarwin then pkgs.unstable.ghostty-bin.terminfo else pkgs.unstable.ghostty.terminfo
    )
  ];
}
