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
    ../programs/looking-glass-client.nix
    ../programs/mpv.nix
    ../programs/sublime-text.nix
    ../programs/wezterm.nix
  ];
}
