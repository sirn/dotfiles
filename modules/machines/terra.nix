{ pkgs, config, ... }:

{
  flatpak.enable = true;

  imports = [
    ../common.nix
    ../common-linux.nix

    # profiles
    ../home/flatpak.nix
    ../home/fonts.nix
    ../home/sway-dark.nix
    ../home/sway.nix

    # programs
    ../programs/1password.nix
    ../programs/ffmpeg.nix
    ../programs/firefox.nix
    ../programs/imagemagick.nix
    ../programs/intellij.nix
    ../programs/mpv.nix
    ../programs/sublime-text.nix
    ../programs/thunderbird.nix
    ../programs/wezterm.nix
    ../programs/yt-dlp.nix

    # services
    ../services/dropbox.nix
    ../services/dropbox-symlinks.nix
  ];
}
