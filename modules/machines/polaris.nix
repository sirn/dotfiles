{
  flatpak.enable = true;
  machine.isLaptop = true;
  machine.desktop.preferDark = true;

  imports = [
    ../common.nix
    ../common-linux.nix

    # profiles
    ../home/breeze.nix
    ../home/flatpak.nix
    ../home/fonts.nix
    ../home/i18n.nix
    ../home/niri.nix
    ../home/sway.nix

    # programs
    ../programs/1password.nix
    ../programs/brightnessctl.nix
    ../programs/ffmpeg.nix
    ../programs/firefox.nix
    ../programs/imagemagick.nix
    ../programs/intellij.nix
    ../programs/mpv.nix
    ../programs/sublime-text.nix
    ../programs/wezterm.nix
    ../programs/yt-dlp.nix

    # services
    ../services/dropbox-symlinks.nix
  ];
}
