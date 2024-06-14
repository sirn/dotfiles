{ pkgs, config, ... }:

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
    ../programs/alacritty.nix
    ../programs/firefox.nix
    ../programs/intellij.nix
    ../programs/looking-glass-client.nix
    ../programs/mpv.nix

    # services
    ../services/dropbox.nix
  ];
}
