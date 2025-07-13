{
  flatpak.enable = true;

  imports = [
    ../common.nix
    ../common-linux.nix

    # profiles
    ../home/flatpak.nix
    ../home/fonts.nix
    ../home/plasma.nix

    # programs
    ../programs/1password.nix
    ../programs/ffmpeg.nix
    ../programs/imagemagick.nix
    ../programs/intellij.nix
    ../programs/krita.nix
    ../programs/mpv.nix
    ../programs/sublime-text.nix
    ../programs/wezterm.nix
    ../programs/yt-dlp.nix

    # services
    ../services/dropbox.nix
    ../services/dropbox-symlinks.nix
  ];
}
