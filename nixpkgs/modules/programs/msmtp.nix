{ config, pkgs, ... }:

let
  inherit (pkgs.stdenv) isDarwin;
in
{
  programs.msmtp = {
    enable = !isDarwin;
  };
}
