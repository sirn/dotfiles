{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    unstable.yt-dlp
  ];
}
