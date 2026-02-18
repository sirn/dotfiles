{
  flatpak.enable = true;

  imports = [
    ../common.nix
    ../common-linux.nix

    # profiles
    ../home/breeze-dark.nix
    ../home/breeze.nix
    ../home/flatpak.nix
    ../home/fonts.nix
    ../home/i18n.nix
    ../home/niri.nix
    ../home/notification.nix
    ../home/sway.nix
    ../home/uwsm.nix

    # programs
    ../programs/1password.nix
    ../programs/ffmpeg.nix
    ../programs/firefox.nix
    ../programs/imagemagick.nix
    ../programs/intellij.nix
    ../programs/ghostty.nix
    ../programs/mpv.nix
    ../programs/sublime-text.nix
    ../programs/virt-manager.nix
    ../programs/yt-dlp.nix
  ];
}
