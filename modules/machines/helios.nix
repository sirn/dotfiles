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
    ../programs/firefox.nix
    ../programs/foot.nix
    ../programs/intellij.nix
    ../programs/looking-glass-client.nix
    ../programs/zed.nix
  ];
}
