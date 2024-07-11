{
  flatpak.enable = true;

  imports = [
    ../common.nix
    ../common-linux.nix

    # profiles
    ../home/flatpak.nix
    ../home/fonts.nix
    ../home/sway.nix

    # programs
    ../programs/firefox.nix
    ../programs/foot.nix
    ../programs/intellij.nix
    ../programs/looking-glass-client.nix
    ../programs/mpv.nix
    ../programs/zed.nix
  ];
}
