{ pkgs, ... }:

{
  programs.yt-dlp = {
    enable = true;
    package = pkgs.unstable.yt-dlp;
  };
}
