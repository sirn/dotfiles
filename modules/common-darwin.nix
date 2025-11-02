{
  imports = [
    ./home/fonts.nix
    ./home/home.nix

    # programs
    ./programs/1password.nix
    ./programs/ffmpeg.nix
    ./programs/firefox.nix
    ./programs/imagemagick.nix
    ./programs/intellij.nix
    ./programs/mpv.nix
    ./programs/sublime-text.nix
    ./programs/yt-dlp.nix

    # services
    ./services/dropbox-symlinks.nix
  ];
}
