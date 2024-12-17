{ pkgs, ... }:

{
  programs.yt-dlp = {
    enable = true;

    # Use unstable for compatibility
    package = pkgs.unstable.yt-dlp;
  };
}
