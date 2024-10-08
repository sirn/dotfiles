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
    ../programs/intellij.nix
    ../programs/krita.nix
    ../programs/thunderbird.nix
    ../programs/wezterm.nix
    ../programs/zed.nix

    # services
    ../services/dropbox.nix
  ];
}
