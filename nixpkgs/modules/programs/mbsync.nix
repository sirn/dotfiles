{ config, pkgs, ... }:

let
  inherit (pkgs.stdenv) isDarwin;
in
{
  programs.mbsync = {
    enable = !isDarwin;
  };
}
