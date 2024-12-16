{ config, pkgs, lib, ... }:

let
  inherit (lib) mkIf;
  inherit (pkgs.stdenv) isLinux;
in
{
  home.packages = with pkgs; [
    (ffmpeg-full.override (if isLinux then {
      withMfx = false;
      withVpl = true;
    } else { }))
  ];
}
