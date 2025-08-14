{
  flatpak.enable = true;

  imports = [
    ../common.nix
    ../common-linux.nix

    # profiles
    ../home/flatpak.nix
    ../home/fonts.nix
    ../home/i18n.nix
    ../home/sway.nix

    # programs
    ../programs/1password.nix
    ../programs/ffmpeg.nix
    ../programs/imagemagick.nix
    ../programs/intellij.nix
    ../programs/looking-glass-client.nix
    ../programs/mpv.nix
    ../programs/sublime-text.nix
    ../programs/wezterm.nix
    ../programs/yt-dlp.nix
  ];
}
