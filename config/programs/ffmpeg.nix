{ config, pkgs, lib, ... }:

{
  home.packages = with pkgs; [
    (ffmpeg-full.override (if pkgs.stdenv.isLinux then {
      withMfx = false;
      withVpl = true;
    } else { }))
  ];
}
